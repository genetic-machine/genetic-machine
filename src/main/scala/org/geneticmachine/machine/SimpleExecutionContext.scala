package org.geneticmachine.machine

import org.geneticmachine.common.graph.Graph
import org.geneticmachine._

import scala.concurrent.Future
import scala.util.{Failure, Success, Try}

class SimpleExecutionContext extends ExecutionContext {

  override implicit val futureExecutionContext = scala.concurrent.ExecutionContext.Implicits.global

  override val logger: org.slf4j.Logger = org.slf4j.LoggerFactory.getLogger(getClass.getSimpleName)

  def submit[I, O, S, C >: this.type <: ExecutionContext ](pair: StepPair[I, O, S, C])
                                                          (implicit db: DBDriver): Future[PairResult[S]] = {
    /** Bind context **/
    val algorithm = pair.algorithm(this)
    val environment = pair.environment(this)

    logger.info {
      s"Executing pair: $pair."
    }

    val parentalGraph: Option[Graph] = for {
      pId <- pair.parentId
    } yield db.load(pId)

    type AlgoState = algorithm.StateT

    logger.info {
      s"Initialization of the algorithm and the environment..."
    }
    val initialAlgoState = algorithm.init(parentalGraph)
    val initialEnvState = environment.init

    def evaluate(algoState: AlgoState, envState: S, input: Option[I]): Future[PairResult[S]] = {
      if (input.isEmpty) {
        finalize(algoState, envState)
      } else {
        for {
          (algoState1, output) <- algorithm.act(algoState, input.get)
          (envState1, input1) <- environment.process(envState, output)
          result <- evaluate(algoState1, envState1, input1)
        } yield result
      }
    }

    def finalize(algoState: AlgoState, envState: S): Future[PairResult[S]] = {
      val result = for {
        (aGraph, ams, acms) <- algorithm.finish(algoState)
        (eGraph, ems, ecms) <- environment.finish(envState)
      } yield {
        logger.info("Finalization...")

        val aGraphWithParent = aGraph.parentInjection {
          if (pair.parentId.isDefined) pair.parentId.get else Graph.nullParent
        }

        val algoId = Try { db.save(aGraphWithParent) }

        algoId match {
          case Failure(t: Throwable) =>
            logger.warn("Algorithm final state was failed to serialize:", t)

          case _ => ()
        }

        for (aId <- algoId) {
          Try {
            db.save(eGraph.uniqueRelationInjection(Graph.experimentRelation, aId))
          } match {
            case Failure(t: Throwable) =>
              logger.warn("Environment final state was failed to serialize:", t)

            case _ => ()
          }
        }

        val fState = FinalState(envState, ams ++ ems, acms ++ ecms)

        PairResult(Success(fState), algoId)
      }

      result.recover {
        case e: Throwable =>
          PairResult(Failure(e), Failure(e))
      }
    }

    for {
      as <- initialAlgoState
      (es, input) <- initialEnvState
      result <- evaluate(as, es, input)
    } yield result
  }
}
