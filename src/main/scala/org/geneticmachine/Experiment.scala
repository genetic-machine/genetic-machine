package org.geneticmachine

import scala.util.Try

/**
 * Represents result of a single environment-algorithm interaction, i.e. one algorithm in one environment.
 *
 * @param finalState final state of the environment.
 * @param metrics dictionary: metric name -> metric value
 * @param continuousMetrics the same as `metrics` except values are represented as sequences of doubles, i.e. vectors.
 */
case class FinalState[+S](finalState: S,  metrics: Map[String, Double], continuousMetrics: Map[String, Seq[Double]])

/**
 * Represents result of one step of an experiment.
 * Utility class to capture possible errors.
 */
case class PairResult[+S](finalState: Try[FinalState[S]], algorithmId: Try[Long]) {
  def isSuccess: Boolean = finalState.isSuccess && algorithmId.isSuccess

  def isFailure = !isSuccess
}

object ExperimentResult {
  def empty: ExperimentResult[Nothing] = {
    ExperimentResult(List.empty)
  }
}

case class ExperimentResult[+S](steps: List[PairResult[S]]) {
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
final case class StepPair[I, O, +S, C]
  (algorithm: AlgorithmGen[I, O, C], environment: EnvironmentGen[I, O, S, C], parentId: Option[Long]) {

  def inherited(result: ExperimentResult[S]): StepPair[I, O, S, C] = {
    if (result.steps.isEmpty) {
      this
    } else {
      inherited(result.lastAlgorithmId)
    }
  }

  def inherited(parentId: Long): StepPair[I, O, S, C] = {
    copy(parentId = Some(parentId))
  }
}

object Experiment {

  type StepNext[I, O, S, C] = Option[(StepPair[I, O, S, C], Experiment[I, O, S, C])]

  type GlobalControl[I, O, S, C] = ExperimentResult[S] => Option[StepPair[I, O, S, C]]

  type LocalControl[I, O, S, C] = PairResult[S] => Option[StepPair[I, O, S, C]]

  import language.implicitConversions

  implicit def pairToStep[I, O, S, C](algoEnv: (AlgorithmGen[I, O, C], EnvironmentGen[I, O, S, C])): StepPair[I, O, S, C] = {
    StepPair(algoEnv._1, algoEnv._2, None)
  }

  implicit def stepPairToSingleStep[I, O, S, C](step: StepPair[I, O, S, C]): SingleStep[I, O, S, C] = {
    single(step)
  }

  implicit def pairToSingleStep[I, O, S, C](algoEnv: (AlgorithmGen[I, O, C],
    EnvironmentGen[I, O, S, C])): SingleStep[I, O, S, C] = {
    single {
      StepPair(algoEnv._1, algoEnv._2, None)
    }
  }

  implicit def asGlobalControl[I, O, S, C](f: LocalControl[I, O, S, C]): GlobalControl[I, O, S, C] = {
    case er: ExperimentResult[S] =>
      f(er.steps.head)
  }

  implicit def asGlobalPredicate[I, O, S, C](p: PairResult[S] => Boolean): ExperimentResult[S] => Boolean = {
    case er: ExperimentResult[S] =>
      p(er.steps.head)
  }

  def single[I, O, S, C](step: StepPair[I, O, S, C]): SingleStep[I, O, S, C] = new SingleStep[I, O, S, C](step)

  def empty[I, O, S, C]: EmptyStep[I, O, S, C] = new EmptyStep[I, O, S, C]

  def infinite[I, O, S, C](step: StepPair[I, O, S, C]): Infinite[I, O, S, C] = new Infinite(step)

  def repeat[I, O, S, C](n: Int)(step: StepPair[I, O, S, C]): SeqStep[I, O, S, C] = SeqStep(List.fill(n)(step))

  def seq[I, O, S, C](steps: Seq[StepPair[I, O, S, C]]): SeqStep[I, O, S, C] = SeqStep(steps.toList)

  def chain[I, O, S, C](n: Int)(step: StepPair[I, O, S, C]): Chain[I, O, S, C] = Chain(List.fill(n)(step))

  def chain[I, O, S, C](steps: Seq[StepPair[I, O, S, C]]): Chain[I, O, S, C] = Chain(steps.toList)

  def controlledBy[I, O, S, C](f: ExperimentResult[S] => Option[StepPair[I, O, S, C]]): GeneralStep[I, O, S, C] = GeneralStep[I, O, S, C](f)

  def until[I, O, S, C](p: ExperimentResult[S] => Boolean)(step: Experiment[I, O, S, C]): Until[I, O, S, C] = new Until(p)(step)

  def If[I, O, S, C](p: ExperimentResult[S] => Boolean)(stepTrue: Experiment[I, O, S, C]): If[I, O, S, C] = new If(p)(stepTrue)
}

import Experiment._

/**
 * The zoo of classes is sacrifice for nice syntax.
 *
 * For example, `IfElse(p)(s1)(s2)` can be rewritten as `If(p) { s1 } orElse { s2 }`
 */
sealed abstract class Experiment[I, O, +S, C]
  extends Serializable {

  def next(result: ExperimentResult[S]): StepNext[I, O, S, C]

  final def then[Q <: S](other: Experiment[I, O, Q, C]): Experiment[I, O, S, C] = {
    new UniteExperiment(this, other)
  }
}

final case class SingleStep[I, O, S, C](stepPair: StepPair[I, O, S, C])
  extends Experiment[I, O, S, C] {
    def next(result: ExperimentResult[S]): StepNext[I, O, S, C] = {
      Some(stepPair.inherited(result), new EmptyStep[I, O, S, C])
    }
}

final class EmptyStep[I, O, S, C] extends Experiment[I, O, S, C] {
  override def next(result: ExperimentResult[S]): StepNext[I, O, S, C] = None
}

final case class Infinite[I, O, S, C](stepPair: StepPair[I, O, S, C])
    extends Experiment[I, O, S, C] {
  override def next(result: ExperimentResult[S]): StepNext[I, O, S, C] = {
    Some(stepPair, this)
  }
}

final case class UniteExperiment[I, O, +S, C]
    (first: Experiment[I, O, S, C], second: Experiment[I, O, S, C])
  extends Experiment[I, O, S, C] {

  override def next(result: ExperimentResult[S]): StepNext[I, O, S, C] = {
    first.next(result).map {
      case (nextStep, rest) =>
        (nextStep, new UniteExperiment[I, O, S, C](rest, second))
    }.orElse {
      second.next(result)
    }
  }
}

final case class SeqStep[I, O, S, C](steps: List[StepPair[I, O, S, C]])
  extends Experiment[I, O, S, C] {

  override def next(result: ExperimentResult[S]): StepNext[I, O, S, C] = {
    steps match {
      case step :: rest =>
        Some((step, SeqStep(rest)))

      case Nil =>
        None
    }
  }
}

final case class Chain[I, O, S, C](steps: List[StepPair[I, O, S, C]])
  extends Experiment[I, O, S, C] {

  override def next(result: ExperimentResult[S]): StepNext[I, O, S, C] = {
    steps match {
      case step :: rest =>
        Some((step.inherited(result), Chain(rest)))

      case Nil =>
        None
    }
  }
}

final class Until[I, O, S, C](val p: ExperimentResult[S] => Boolean)
                             (val step: Experiment[I, O, S, C])
  extends Experiment[I, O, S, C] {

  override def next(result: ExperimentResult[S]): StepNext[I, O, S, C] = {
    if (!p(result)) {
      step.next(result)
    } else {
      None
    }
  }
}

final class IfElse[I, O, S, C](val p: ExperimentResult[S] => Boolean)
                              (val stepTrue: Experiment[I, O, S, C])
                              (val stepFalse: Experiment[I, O, S, C])
  extends Experiment[I, O, S, C] {
  override def next(result: ExperimentResult[S]): StepNext[I, O, S, C] = {
    if (p(result)) {
      stepTrue.next(result)
    } else {
      stepFalse.next(result)
    }
  }
}

final class If[I, O, S, C](val p: ExperimentResult[S] => Boolean)
                          (val stepTrue: Experiment[I, O, S, C])
  extends Experiment[I, O, S, C] {

  override def next(result: ExperimentResult[S]): StepNext[I, O, S, C] = {
    if (p(result)) {
      stepTrue.next(result)
    } else {
      None
    }
  }

  def orElse(stepFalse: Experiment[I, O, S, C]): IfElse[I, O, S, C] = {
    new IfElse[I, O, S, C](p)(stepTrue)(stepFalse)
  }
}

final case class GeneralStep[I, O, S, C]
    (f: ExperimentResult[S] => Option[StepPair[I, O, S, C]])
  extends Experiment[I, O, S, C] {

  override def next(result: ExperimentResult[S]): StepNext[I, O, S, C] = {
    for {
      step <- f(result)
    } yield (step, new GeneralStep(f))
  }
}