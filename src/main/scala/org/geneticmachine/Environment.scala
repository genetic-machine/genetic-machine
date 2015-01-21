package org.geneticmachine

import org.geneticmachine.common.graph.Graph

import scala.concurrent.Future

abstract class EnvironmentGen[+InputT, -OutputT, StateT, -Context <: ExecutionContext] {
  def apply[C <: Context](c: C): Environment[InputT, OutputT, StateT, C]
}

abstract class Environment[+InputT, -OutputT, StateT, +Context <: ExecutionContext]
  extends Serializable {

  val metrics: Seq[Metric[StateT]] = Seq.empty

  val continuousMetrics: Seq[ContinuousMetric[StateT]] = Seq.empty

  val context: Context

  /**
   * @return initial status and initial algorithm's input.
   */
  def init: Future[(StateT, Option[InputT])]

  /**
   * The "physics" of the environment.
   *
   * Applies changes by algorithm's action to the world's state.
   *
   * @param state current status of environment
   * @param algorithmAction algorithm's response
   * @return `Some(...)`, which includes new status of environment and new input for the algorithm,
   *        if goal is not achieved, otherwise `None`
   */
  def process(state: StateT, algorithmAction: OutputT): Future[(StateT, Option[InputT])]

  def serialize(state: StateT): Future[Graph]

  /**
   * Releases all occupied resources, if any.
   */
  def reset(): Future[Unit] = Future.successful { () }

  type EnvironmentResult = (Graph, Map[String, Double], Map[String, Seq[Double]])

  final def finish(state: StateT)(implicit ex: concurrent.ExecutionContext): Future[EnvironmentResult] = {
    reset().flatMap { _ =>
      serialize(state).zip {
        AbstractMetric.sequence(metrics, state)
      }.zip {
        AbstractMetric.sequence(continuousMetrics, state)
      }.map {
        case ((graph, ms), cms) =>
          (graph, ms, cms)
      }
    }
  }
}
