package robot.labyrinth

import robot.Robot
import environment.labyrinth.{CellStatus, Point, Labyrinth}
import robot.labyrinth.Command.Command
import robot.labyrinth.Direction.Direction
import akka.actor.ActorRef
import breeze.linalg.DenseMatrix

class LabyrinthRobot(brain: ActorRef) extends
  Robot[(Labyrinth, Command), Command,
        (Labyrinth, Direction, Point),
        Labyrinth, (Point, Int), Point] (brain) {

  val sensorDeep = 5
  var labyrinth: Labyrinth = DenseMatrix.fill(0, 0)(CellStatus.Unknown)
  var robotPosition: Point = Point(0, 0)
  var robotDirection: Direction = Direction.North
  var goal: Point = Point(0, 0)

  def changeStatus(command: Command): Option[(Point, Int)] = {
    command match {
      case Command.Forward =>
        robotPosition += robotDirection
      case Command.TurnLeft =>
        robotPosition *= Direction.North
      case Command.TurnRight =>
        robotPosition *= Direction.South
    }

    if (robotPosition == goal) {
      None
    } else {
      Some(robotPosition, sensorDeep)
    }
  }

  def applyObservation(data: Labyrinth, from: (Point, Int)): (Labyrinth, Command) = {
    val (Point(fromX, fromY), deep) = from
    for {
      x <- 0 until data.rows
      y <- 0 until data.cols
      labX = x - deep + fromX
      labY <- y - deep + fromY
      if Point(labX, labY).inBorders(labyrinth.rows, labyrinth.cols)
    } {
      labyrinth(labX, labY) = data(x, y)
    }

    val prediction = minPathSensor(labyrinth, robotPosition, goal, robotDirection)
    (labyrinth, prediction)
  }

  def currentStatus: (Labyrinth, Direction, Point) = {
    (labyrinth, robotDirection, robotPosition)
  }

  def statusReset(params: Point): (Point, Int) = {
    labyrinth = DenseMatrix.fill(params.x, params.y)(CellStatus.Unknown)
    robotPosition = Point(0, params.y / 2)
    goal = Point(params.x - 1, params.y / 2)
    robotDirection = Direction.East

    (robotPosition, params.l1Norm)
  }
}
