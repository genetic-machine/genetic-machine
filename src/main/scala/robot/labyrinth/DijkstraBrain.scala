package robot.labyrinth

import robot.Brain
import akka.actor.ActorRef
import Direction.Direction
import Command.Command

case class DijkstraInput(lab: Labyrinth, robotPosition: Point, robotDirection: Direction, goal: Point)

class DijkstraBrain extends  Brain[DijkstraInput, Command] {

  override def doIntelligence(sensor: ActorRef, actuator: ActorRef): Receive = {
    case Brain.Input(data: DijkstraInput) if context.sender == sensor =>
      val DijkstraInput(lab: Labyrinth, from: Point, robotDirection: Direction, goal: Point) = data
      val optimalResponse = minPathSensor(lab, from, robotDirection, goal).keys.toList(0)
      actuator ! Brain.Output(optimalResponse)
      log.info(s"Sent: ${Brain.Output(optimalResponse)}")

    case Brain.Reset =>
  }
}
