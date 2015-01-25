package org.geneticmachine.navigation.algorithm

import scala.concurrent.Future
import breeze.stats.distributions._

import org.geneticmachine.navigation._

import org.geneticmachine.{Algorithm, ExecutionContext, AlgorithmGen}
import org.geneticmachine.common.graph.Graph

object BrownianWalker extends AlgorithmGen[NavigationInput, NavigationOutput, ExecutionContext] {
  override def toString: String = "Brownian Walker"

  override def apply(c: ExecutionContext): Algorithm[NavigationInput, NavigationOutput] = new BrownianWalker(c)

  def empty: Graph = Graph.empty(Graph.algorithmLabel, "NavigationInput", "NavigationOutput")
}

class BrownianWalker(protected val context: ExecutionContext)
  extends Algorithm[NavigationInput, NavigationCommand] {

  type Context = ExecutionContext

  type StateT = Int

  val commandDist: Rand[Int] = for {
    cId <- Rand.randInt(3)
  } yield cId

  override def serialize(state: Int) = Future.successful {
    BrownianWalker.empty
  }

  override def init(parent: Option[Graph]): Future[Int] = Future.successful { 0 }

  override def act(stepCounter: Int, data: NavigationInput): Future[(Int, NavigationCommand)] = {
    Future.successful {
      val command: NavigationCommand = commandDist.draw()
      val newState: Int = stepCounter + 1
      (newState, command)
    }
  }

  override def reset(state: Int) = Future.successful(state)
}
