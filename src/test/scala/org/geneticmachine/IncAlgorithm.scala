package org.geneticmachine

import org.geneticmachine.common.graph.Graph
import org.geneticmachine.ExecutionContext

import scala.concurrent.Future

object IncAlgorithmGen extends AlgorithmGen[Int, Int, ExecutionContext] {
  def apply[C <: ExecutionContext](context: C): Algorithm[Int, Int, ExecutionContext] = {
    new IncAlgorithm(context)
  }
}

final class IncAlgorithm(val context: ExecutionContext)
  extends Algorithm[Int, Int, ExecutionContext] {

  import Graph._

  type StateT = List[Int]

  override def init(parent: Option[Graph]): Future[StateT] = Future.successful {
    val parentHistory = for {
      graph <- parent
      state <- graph(graph.inputNodeId).props.getAs[Array[Int]]("history")
    } yield state.toList

    parentHistory.getOrElse(List.empty)
  }

  override def act(state: List[Int], x: Int): Future[(List[Int], Int)] = {
    Future.successful { (x :: state, x + 1) }
  }

  override def serialize(state: List[Int]): Future[Graph] = Future.successful {
    val props = Map("history", state.toArray)
    Graph.single(Graph.algorithmLabel)("DummyAlgorithm", props)
  }
}
