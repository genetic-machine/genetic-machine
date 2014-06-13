package robot.labyrinth

import robot.Robot
import akka.actor.ActorRef
import scala.concurrent.Future
import breeze.linalg.DenseMatrix
import robot.labyrinth.vision._
import robot.labyrinth.generators._

case class LabyrinthStatus(visionMap: Labyrinth, labyrinth: Labyrinth,
                           robotPosition: Point, robotDirection: Direction.Direction,
                           goal: Point) {

  def printVisionMap: DenseMatrix[Char] = {
    val result = printLab(visionMap)
    result(robotPosition.x, robotPosition.y) = robotDirection match {
      case Direction.North => 'V'
      case Direction.South => 'A'
      case Direction.West => '>'
      case Direction.East => '<'
    }

    result
  }
}

class LabyrinthRobot(brain: ActorRef, val labyrinthGen: LabyrinthGenerator, val vision: Vision)
  extends Robot[DijkstraInput, LabyrinthStatus, Command.Command](brain) {

  import context.dispatcher

  def selfSetup() = Future {
    val lab = labyrinthGen()
    val start = Point(0, (lab.cols - 1) / 2)
    val goal = Point(lab.rows - 1, (lab.cols - 1) / 2)
    val obs = vision(lab, start)
    val visionMap = obs.impose(Labyrinth.unknown(lab.rows, lab.cols))

    val status = LabyrinthStatus(visionMap, lab, start, Direction.North, goal)
    val initialInput = DijkstraInput(visionMap, start, Direction.North, goal)

    (status, initialInput)
  }

  def processOutput(status: LabyrinthStatus, brainOutput: Command.Command): Option[(LabyrinthStatus, DijkstraInput)] = {
    val (newPosition, newDirection) = brainOutput match {
      case Command.Forward if (status.robotPosition + status.robotDirection).inLabyrinth(status.labyrinth) =>
        (status.robotPosition + status.robotDirection, status.robotDirection)
      case Command.Forward =>
        (status.robotPosition, status.robotDirection)
      case Command.TurnLeft =>
        (status.robotPosition, status.robotDirection.turnLeft)
      case Command.TurnRight =>
        (status.robotPosition, status.robotDirection.turnRight)
    }


    val obs = vision(status.labyrinth, newPosition)
    obs.impose(status.visionMap)
    val newStatus = LabyrinthStatus(status.visionMap, status.labyrinth, newPosition, newDirection, status.goal)
    val newInput = DijkstraInput(status.visionMap, newPosition, newDirection, status.goal)

    if (newPosition == status.goal) {
      None
    } else {
      Some((newStatus, newInput))
    }
  }

  def training(status: LabyrinthStatus) = guideBrain(status)
}
