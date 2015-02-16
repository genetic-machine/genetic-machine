package org.geneticmachine.navigation.algorithm

import org.geneticmachine.ExecutionContext
import org.geneticmachine.common.graph.Graph
import org.geneticmachine.navigation._

import scala.concurrent.Future

object HumanControl extends NavigationAlgorithmGen[ExecutionContext] {
  override def apply(c: ExecutionContext): NavigationAlgorithm = {
    new HumanControl(c)
  }
}

class HumanControl(context: ExecutionContext) extends NavigationAlgorithm {
  type StateT = Int

  val log = context.logger
  import context.futureExecutionContext

  override def init(parent: Option[Graph]): Future[Int] = Future.successful(0)

  private val commandMap = Map[Char, NavigationCommand] (
    'a' -> NavigationCommand.TurnLeft,
    'w' -> NavigationCommand.Forward,
    'd' -> NavigationCommand.TurnRight
  )

  private def getCommand(): NavigationCommand = {
    val c = Console.in.read().toChar
    if (commandMap.contains(c)) {
      commandMap(c)
    } else {
      getCommand()
    }
  }

  override def act(state: Int, inputData: NavigationInput): Future[(Int, NavigationCommand)] = Future {
    val obs = inputData.observation.orientated
    val cm = labToCharMatrix(obs.visionMap)
    val rp = obs.from
    cm(rp.point.x, rp.point.y) = Direction.char(rp.direction)
    log.info(s"Input on $state")
    log.info(s"\n${ matrixToString(cm) }")

    (state + 1, getCommand())
  }

  override def serialize(state: Int) = Future.successful {
    log.info(s"Reset on $state")
    Graph.empty(Graph.algorithmLabel, "NavigationInput", "NavigationOutput")
  }

  override def reset(state: Int) = Future.successful {
    log.info(s"Reset on $state")
    state
  }
}
