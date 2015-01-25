package org.geneticmachine

import org.geneticmachine.common.graph.Graph

import scala.concurrent.Future

abstract class AlgorithmGen[-InputT, +OutputT, -Context <: ExecutionContext] {
  def apply(c: Context): Algorithm[InputT, OutputT]

  def errorGraph(e: Throwable): Graph = Graph.errorGraph(Graph.algorithmLabel, e)
}

abstract class Algorithm[-InputT, +OutputT] {
  type StateT

  val metrics: Seq[Metric[StateT]] = Seq.empty
  val continuousMetrics: Seq[ContinuousMetric[StateT]] = Seq.empty

  /**
   * Returns initial state of algorithm.
   *
   * Serialized parent provided if there is any.
   *
   */
  def init(parentalGraph: Option[Graph]): Future[StateT]

  /**
   * Processes input data.
   * @param state current state of brain.
   * @param inputData incoming data.
   * @return the pair of new brain's state and response.
   */
  def act(state: StateT, inputData: InputT): Future[(StateT, OutputT)]

  /**
   * Releases all occupied resources and clears algorithm's state before passing it to the next generation.
   *
   * By default, returns input state.
   *
   * @param state current state.
   * @return cleared state.
   */
  def reset(state: StateT): Future[StateT] = Future.successful(state)

  /**
   * Saves current state into [[org.geneticmachine.common.graph.Graph]].
   * @param state current state.
   * @return serialized state.
   */
  def serialize(state: StateT): Future[Graph]

  type AlgorithmResult = (Graph, Map[String, Double], Map[String, Seq[Double]])

  /**
   * Resets and serializes final state of the algorithm and calculates metrics and continuous metrics.
   *
   * All actions are in parallel.
   *
   * @return serialized state, metrics' values, continuous metrics' values.
   */
  final def finish(state: StateT)(implicit ex: concurrent.ExecutionContext): Future[AlgorithmResult] = {
    val finalState = reset(state)

    val graph = finalState.flatMap(serialize)

    val msF = Future.sequence {
      for {
        m <- metrics
      } yield for {
        state <- finalState
      } yield (m.metricName, m(state))
    }

    val cmsF = Future.sequence {
      for {
        cm <- continuousMetrics
      } yield for {
        state <- finalState
      } yield (cm.metricName, cm(state))
    }

    for {
      g <- graph
      ms <- msF
      cms <- cmsF
    } yield (g, ms.toMap, cms.toMap)
  }
}