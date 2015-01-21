package org.geneticmachine

import org.scalatest.{Matchers, FlatSpec}

import scala.util.{Success, Try}

class ExperimentTest extends FlatSpec with Matchers {
  behavior of "Experiment"

  it must "generate proper sequence" in {
    import Experiment._

    val experiment = {
      (IncAlgorithmGen -> IntEnvironmentGen).then {
        until { er: ExperimentResult[List[Int]] =>
          er.steps.size > 10
        }(IncAlgorithmGen -> IntEnvironmentGen)
      }
    }

    type Executor[I, O, S, C] = (ExperimentResult[S], StepPair[I, O, S, C]) => ExperimentResult[S]

    def foldExperiment[I, O, S, C](executor: Executor[I, O, S, C])
                                  (ex: Experiment[I, O, S, C],
                                   er: ExperimentResult[S] = ExperimentResult.empty): List[StepPair[I, O, S, C]] = {
      val nextOpt = ex.next(er)

      if (nextOpt.isDefined) {
        val (pair, next) = nextOpt.get
        val nEr = executor(er, pair)

        pair :: foldExperiment(executor)(next, nEr)
      } else {
        Nil
      }
    }

    def intExecutor(er: ExperimentResult[List[Int]], pair: StepPair[Int, Int, List[Int], ExecutionContext]): ExperimentResult[List[Int]] ={
      val finalState = FinalState(List(1, 2, 3), Map.empty, Map.empty)
      val lastAlgoId = Try { er.lastAlgorithmId }.getOrElse(100l)
      val stepResult = PairResult(Success(finalState), Success(lastAlgoId + 10))

      ExperimentResult(stepResult :: er.steps)
    }

    foldExperiment(intExecutor)(experiment)
  }
}
