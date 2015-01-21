package org.geneticmachine

import org.geneticmachine.common.graph.Graph

import scala.concurrent.Future

object IntEnvironmentGen extends EnvironmentGen[Int, Int, List[Int], ExecutionContext] {
  def apply[C <: ExecutionContext](c: C): IntEnvironment = {
    new IntEnvironment(10)(c)
  }
}

final class IntEnvironment(val max: Int)
                          (val context: ExecutionContext)
  extends Environment[Int, Int, List[Int], ExecutionContext] {

  def input(): Int = {
    scala.util.Random.nextInt(100)
  }

  def init: Future[(List[Int], Option[Int])] = Future.successful {
    val i = input()

    (List(i), Some(i))
  }

  def process(state: List[Int], algorithmAction: Int): Future[(List[Int], Option[Int])] = Future.successful {
    if (algorithmAction == state.head && state.size < max) {
      val i = input()

      (i :: state, Some(i))
    } else {
      (state, None)
    }
  }

  def serialize(status: List[Int]): Future[Graph] = Future.successful {
    val props = Map("history", status.toArray)

    Graph.single(Graph.environmentLabel)("IntEnvironment", props)
  }
}
