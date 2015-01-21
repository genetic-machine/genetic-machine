package org.geneticmachine.navigation.algorithm

import org.geneticmachine._
import org.geneticmachine.common.graph._
import org.geneticmachine.navigation._
import org.geneticmachine.navigation.heuristics.DijkstraSense

import scala.concurrent.Future

object NaiveNavigation extends AlgorithmGen[NavigationInput, NavigationOutput, ExecutionContext] {
  override def toString: String = "Naive Dijkstra Algorithm"

  override def apply[C <: ExecutionContext](c: C): Algorithm[NavigationInput, NavigationCommand, ExecutionContext] = {
    new NaiveNavigation(c)
  }

  def empty: Graph = Graph.empty(Graph.algorithmLabel, "NavigationInput", "NavigationOutput")
}

class NaiveNavigation(val context: ExecutionContext)
  extends Algorithm[NavigationInput, NavigationCommand, ExecutionContext] {

  type StateT = Int

  override def serialize(state: Int) = Future.successful {
    NaiveNavigation.empty
  }

  override def init(g: Option[Graph]): Future[Int] = Future.successful(0)

  import context.futureExecutionContext

  override def act(stepCounter: Int, data: NavigationInput): Future[(Int, NavigationCommand)] = Future {
    val command = DijkstraSense.optimal(data)
    val newState = stepCounter + 1
    (newState, command)
  }

  override def reset(state: Int) = Future.successful(state)
}
