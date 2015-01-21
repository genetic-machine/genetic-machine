package org.geneticmachine.machine

import org.geneticmachine._

import scala.concurrent.Future

final class SimpleMachine[+C <: ExecutionContext]
    (implicit val executionContext: C, implicit val db: DBDriver)
  extends Machine[C] {

  def submit[I, O, S, C1 >: C](experiment: Experiment[I, O, S, C1]): Future[ExperimentResult[S]] = {
    def foldExperiment(ex: Experiment[I, O, S, C1], acc: List[PairResult[S]]): Future[ExperimentResult[S]] = {
      val nextOpt = ex.next(ExperimentResult(acc))

      if (nextOpt.isDefined) {
        val (pair, nextPart) = nextOpt.get
        val pairResult = executionContext.submit(pair)(db)

        pairResult.flatMap { pr: PairResult[S] =>
          val acc1 = pr :: acc
          foldExperiment(nextPart, acc1)
        }
      } else {
        Future.successful(ExperimentResult(acc))
      }
    }

    foldExperiment(experiment, List.empty)
  }
}
