package robot.labyrinth

import robot.Brain
import akka.actor.ActorRef
import Direction.Direction
import Command.Command

import scala.concurrent.Future

case class DijkstraInput(lab: Labyrinth, robotPosition: Point, robotDirection: Direction, goal: Point)

class DijkstraBrain extends  Brain[DijkstraInput, Command] {

  import context.dispatcher

  override def think(data: DijkstraInput): Future[Command] = Future {
    val DijkstraInput(lab, from: Point, robotDirection: Direction, goal: Point) = data
    minPathSensor(lab, from, robotDirection, goal).keys.toList(0)
  }

  override def reset() = Future {}
  override def copyTo(other: ActorRef) = Future {}
}
