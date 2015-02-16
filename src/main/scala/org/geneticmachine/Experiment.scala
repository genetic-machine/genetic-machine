package org.geneticmachine

import scala.util.Try

/**
 * Represents result of a single environment-algorithm interaction, i.e. one algorithm in one environment.
 *
 * @param finalState final state of the environment.
 * @param metrics dictionary: metric name -> metric value
 * @param continuousMetrics the same as `metrics` except values are represented as sequences of doubles, i.e. vectors.
 */
case class FinalState[+S](finalState: S,
                          metrics: Map[String, Double],
                          continuousMetrics: Map[String, Seq[Double]]) extends Serializable {
  override def toString: String = {
    val metricsRepr = for {
      (k, v) <- metrics
    } yield s"$k: $v"

    val cMetricsRepr = for {
      (k, vs) <- continuousMetrics
    } yield s"$k: [${vs.mkString(", ")}]"

    s"$finalState {${(metricsRepr ++ cMetricsRepr).mkString(", ")}}"
  }
}

/**
 * Represents result of one step of an experiment.
 * Utility class to capture possible errors.
 */
case class PairResult[+S]
    (finalState: Try[FinalState[S]], algorithmId: Try[Long])
  extends Serializable {

  def isSuccess: Boolean = finalState.isSuccess && algorithmId.isSuccess

  def isFailure = !isSuccess

  override def toString: String = {
    val fsRepr = finalState.map { fs => fs.toString }.getOrElse("NONE")
    val algoIdRepr = algorithmId.map { id => id.toString }.getOrElse("NONE")
    s"(finalState: $fsRepr; DB id: $algoIdRepr)"
  }
}

object ExperimentResult {
  def empty: ExperimentResult[Nothing] = {
    ExperimentResult(List.empty)
  }
}

case class ExperimentResult[+S](steps: List[PairResult[S]]) {

  override def toString: String = {
    steps.mkString("Experiment result {\n  ", "\n  ", "\n}")
  }

  def head: PairResult[S] = steps.head

  def lastSuccess: Option[PairResult[S]] = {
    steps.dropWhile { cr =>
      cr.isFailure
    }.headOption
  }

  def lastSuccessAlgorithmId: Option[Long] = {
    for {
      cr <- lastSuccess
      id <- cr.algorithmId.toOption
    } yield id
  }

  def lastAlgorithmId: Long = {
    steps.head.algorithmId.get
  }
}

/**
 * It isn't nice to operate with _1 and _2 methods of [[Tuple2]].
 *
 * There is the implicit conversion from pair to StepPair in top-level package object [[org.geneticmachine]].
 *
 * @param parentId if matches Some(id), means load exactly this id. None means generate new.
 */
final case class StepPair[I, O, S, -C <: ExecutionContext]
    (algorithm: AlgorithmGen[I, O, C], environment: EnvironmentGen[I, O, S, C], parentId: Option[Long] = None)
  extends Serializable {

  def inherited(result: ExperimentResult[S]): StepPair[I, O, S, C] = {
    if (result.steps.isEmpty) {
      this
    } else {
      inherited(result.lastAlgorithmId)
    }
  }

  def inherited(newParentId: Long): StepPair[I, O, S, C] = {
    copy(parentId = Some(newParentId))
  }

  def inherited(newParentId: Option[Long]): StepPair[I, O, S, C] = {
    copy(parentId = newParentId.orElse(parentId))
  }

  override def toString: String = s"Pair: $algorithm (parent: $parentId) -> $environment"
}

object Experiment {

  type StepNext[I, O, S, -C <: ExecutionContext] = Option[(StepPair[I, O, S, C], Experiment[I, O, S, C])]

  type GlobalControl[I, O, S, -C <: ExecutionContext] = ExperimentResult[S] => Option[StepPair[I, O, S, C]]

  type LocalControl[I, O, S, -C <: ExecutionContext] = PairResult[S] => Option[StepPair[I, O, S, C]]

  import language.implicitConversions

  implicit def pairToStep[I, O, S, C <: ExecutionContext](algoEnv: (AlgorithmGen[I, O, C], EnvironmentGen[I, O, S, C])): StepPair[I, O, S, C] = {
    StepPair(algoEnv._1, algoEnv._2, None)
  }

  def step[I, O, S, C <: ExecutionContext](algoEnv: (AlgorithmGen[I, O, C], EnvironmentGen[I, O, S, C])): StepPair[I, O, S, C] = {
    StepPair(algoEnv._1, algoEnv._2, None)
  }

  implicit def stepPairToSingleStep[I, O, S, C <: ExecutionContext](step: StepPair[I, O, S, C]): SingleStep[I, O, S, C] = {
    single(step)
  }

  implicit def pairToSingleStep[I, O, S, C <: ExecutionContext](algoEnv: (AlgorithmGen[I, O, C],
    EnvironmentGen[I, O, S, C])): SingleStep[I, O, S, C] = {
    single {
      StepPair(algoEnv._1, algoEnv._2, None)
    }
  }

  implicit def asGlobalControl[I, O, S, C <: ExecutionContext](f: LocalControl[I, O, S, C]): GlobalControl[I, O, S, C] = {
    case er: ExperimentResult[S] =>
      f(er.steps.head)
  }

  implicit def asGlobalPredicate[S](p: PairResult[S] => Boolean): ExperimentResult[S] => Boolean = {
    case er: ExperimentResult[S] =>
      p(er.steps.head)
  }

  def startingWith[I, O, S, C <: ExecutionContext](parentId: Long)(experiment: Experiment[I, O, S, C]): StartingWith[I, O, S, C] = {
    new StartingWith(Some(parentId))(experiment)
  }

  def startingWith[I, O, S, C <: ExecutionContext](parentId: Option[Long])(experiment: Experiment[I, O, S, C]): StartingWith[I, O, S, C] = {
    new StartingWith(parentId)(experiment)
  }

  def single[I, O, S, C <: ExecutionContext](step: StepPair[I, O, S, C]): SingleStep[I, O, S, C] = new SingleStep[I, O, S, C](step)

  def empty[I, O, S, C <: ExecutionContext]: FinishStep[I, O, S, C] = new FinishStep[I, O, S, C]

  def finish[I, O, S, C <: ExecutionContext]: FinishStep[I, O, S, C] = new FinishStep[I, O, S, C]

  def infinite[I, O, S, C <: ExecutionContext](step: StepPair[I, O, S, C]): InfiniteSeq[I, O, S, C] = new InfiniteSeq(step)

  def repeat[I, O, S, C <: ExecutionContext](n: Int)(step: StepPair[I, O, S, C]): Sequence[I, O, S, C] = Sequence(List.fill(n)(step))

  def sequence[I, O, S, C <: ExecutionContext](steps: Seq[StepPair[I, O, S, C]]): Sequence[I, O, S, C] = Sequence(steps.toList)

  def chain[I, O, S, C <: ExecutionContext](n: Int)(step: StepPair[I, O, S, C]): Chain[I, O, S, C] = Chain(List.fill(n)(step))

  def chain[I, O, S, C <: ExecutionContext](steps: Seq[StepPair[I, O, S, C]]): Chain[I, O, S, C] = Chain(steps.toList)

  def controlledBy[I, O, S, C <: ExecutionContext](f: ExperimentResult[S] => Option[StepPair[I, O, S, C]]): GeneralStep[I, O, S, C] = GeneralStep[I, O, S, C](f)

  def until[I, O, S, C <: ExecutionContext](p: ExperimentResult[S] => Boolean)(step: Experiment[I, O, S, C]): Until[I, O, S, C] = new Until(p)(step)

  def While[I, O, S, C <: ExecutionContext](p: ExperimentResult[S] => Boolean)(step: Experiment[I, O, S, C]): Until[I, O, S, C] = {
    new Until({ er: ExperimentResult[S] => !p(er) })(step)
  }

  def If[I, O, S, C <: ExecutionContext](p: ExperimentResult[S] => Boolean)(stepTrue: Experiment[I, O, S, C]): If[I, O, S, C] = new If(p)(stepTrue)
}

import Experiment._

/**
 * The zoo of classes is sacrifice for nice syntax.
 *
 * For example, `IfElse(p)(s1)(s2)` can be rewritten as `If(p) { s1 } orElse { s2 }`
 */
sealed abstract class Experiment[I, O, S, -C <: ExecutionContext]
  extends Serializable {

  def next(result: ExperimentResult[S]): StepNext[I, O, S, C]

  def scheme: String

  def scheme(ind: Int): String = scheme.split("\n").map { s => "  " + s}.mkString("\n")

  final def andThen[C1 <: C](other: Experiment[I, O, S, C1]): Experiment[I, O, S, C1] = {
    new UniteExperiment(this, other)
  }
}

final case class SingleStep[I, O, S, -C <: ExecutionContext](stepPair: StepPair[I, O, S, C])
  extends Experiment[I, O, S, C] {

  def scheme: String = s"single($stepPair)"

  def next(result: ExperimentResult[S]): StepNext[I, O, S, C] = {
    Some(stepPair.inherited(result), new FinishStep[I, O, S, C])
  }
}

final class FinishStep[I, O, S, -C <: ExecutionContext] extends Experiment[I, O, S, C] {

  def scheme: String = s"finish"

  def next(result: ExperimentResult[S]): StepNext[I, O, S, C] = None
}

final case class InfiniteSeq[I, O, S, -C <: ExecutionContext](stepPair: StepPair[I, O, S, C])
  extends Experiment[I, O, S, C] {

  def scheme: String = s"infiniteSequence($stepPair)"

  def next(result: ExperimentResult[S]): StepNext[I, O, S, C] = {
    Some(stepPair, this)
  }
}

final case class InfiniteChain[I, O, S, -C <: ExecutionContext](stepPair: StepPair[I, O, S, C])
  extends Experiment[I, O, S, C] {

  def scheme: String = s"infiniteChain($stepPair)"

  def next(result: ExperimentResult[S]): StepNext[I, O, S, C] = {
    Some(stepPair.inherited(result), this)
  }
}

final case class UniteExperiment[I, O, S, -C <: ExecutionContext]
    (first: Experiment[I, O, S, C], second: Experiment[I, O, S, C])
  extends Experiment[I, O, S, C] {

  def scheme: String = s"${first.scheme};\n${second.scheme}"

  def next(result: ExperimentResult[S]): StepNext[I, O, S, C] = {
    first.next(result).map {
      case (nextStep, rest) =>
        (nextStep, new UniteExperiment[I, O, S, C](rest, second))
    }.orElse {
      second.next(result)
    }
  }
}

final case class Sequence[I, O, S, -C <: ExecutionContext](steps: List[StepPair[I, O, S, C]])
  extends Experiment[I, O, S, C] {

  def scheme: String = "Sequence" + {
    (for {
      s <- steps
    } yield s"  single($s);").mkString(": {\n", "\n", "\n}")
  }

  def next(result: ExperimentResult[S]): StepNext[I, O, S, C] = {
    steps match {
      case step :: rest =>
        Some((step, Sequence(rest)))

      case Nil =>
        None
    }
  }
}

final case class Chain[I, O, S, -C <: ExecutionContext](steps: List[StepPair[I, O, S, C]])
  extends Experiment[I, O, S, C] {

  def scheme: String = "chain" + {
    (for {
      s <- steps
    } yield s"  single($s);").mkString(": {\n", "\n", "}")
  }

  def next(result: ExperimentResult[S]): StepNext[I, O, S, C] = {
    steps match {
      case step :: rest =>
        Some((step.inherited(result), Chain(rest)))

      case Nil =>
        None
    }
  }
}

final class Until[I, O, S, -C <: ExecutionContext](val p: ExperimentResult[S] => Boolean)
                                                  (val step: Experiment[I, O, S, C])
  extends Experiment[I, O, S, C] {

  def scheme: String = s"until(...) {\n${step.scheme(2)}\n}"

  def next(result: ExperimentResult[S]): StepNext[I, O, S, C] = {
    if (!p(result)) {
      step.next(result)
    } else {
      None
    }
  }
}

final class IfElse[I, O, S, -C <: ExecutionContext](val p: ExperimentResult[S] => Boolean)
                                                    (val stepTrue: Experiment[I, O, S, C])
                                                    (val stepFalse: Experiment[I, O, S, C])
  extends Experiment[I, O, S, C] {

  def scheme: String = s"if(...) {\n${stepTrue.scheme(2)}\n} else {\n${stepFalse.scheme(2)}\n}"

  def next(result: ExperimentResult[S]): StepNext[I, O, S, C] = {
    if (p(result)) {
      stepTrue.next(result)
    } else {
      stepFalse.next(result)
    }
  }
}

final class If[I, O, S, -C <: ExecutionContext](val p: ExperimentResult[S] => Boolean)
                                               (val stepTrue: Experiment[I, O, S, C])
  extends Experiment[I, O, S, C] {

  def scheme: String = s"if(...) {\n${stepTrue.scheme(2)}\n}"

  def next(result: ExperimentResult[S]): StepNext[I, O, S, C] = {
    if (p(result)) {
      stepTrue.next(result)
    } else {
      None
    }
  }

  def orElse[C1 <: C](stepFalse: Experiment[I, O, S, C1]): IfElse[I, O, S, C1] = {
    new IfElse[I, O, S, C1](p)(stepTrue)(stepFalse)
  }
}

final class StartingWith[I, O, S, -C <: ExecutionContext](val parent: Option[Long])
                                                         (val experiment: Experiment[I, O, S, C])
    extends Experiment[I, O, S, C] {

  def scheme: String = s"startingWith($parent) {\n${experiment.scheme(2)}\n}"

  def next(result: ExperimentResult[S]): StepNext[I, O, S, C] = {
    for {
      (step, rest) <- experiment.next(result)
    } yield (step.inherited(parent), rest)
  }
}

final case class GeneralStep[I, O, S, -C <: ExecutionContext]
    (f: ExperimentResult[S] => Option[StepPair[I, O, S, C]])
  extends Experiment[I, O, S, C] {

  def scheme: String = s"controlled by $f"

  def next(result: ExperimentResult[S]): StepNext[I, O, S, C] = {
    for {
      step <- f(result)
    } yield (step, new GeneralStep(f))
  }
}