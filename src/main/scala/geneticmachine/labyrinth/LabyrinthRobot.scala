package geneticmachine.labyrinth

import geneticmachine.Robot
import akka.actor.ActorRef
import geneticmachine.dataflow.DataFlowFormat
import scala.concurrent.Future
import geneticmachine.labyrinth.vision._
import geneticmachine.labyrinth.generators._

class LabyrinthRobot(brain: ActorRef, val labyrinthGen: LabyrinthGenerator, val vision: Vision)
  extends Robot[LabyrinthInput, LabyrinthStatus, LabyrinthCommand.LabyrinthCommand, LabyrinthScore](brain) {

  import context.dispatcher

  override def init = {
    val (lab, start, goal) = labyrinthGen()

    val obs = vision(lab, start)
    val visionMap = obs.impose(Labyrinth.unknown(lab.rows, lab.cols))

    val path = List(start)
    val history = List()

    val status = LabyrinthStatus(visionMap, lab, start, Direction.North, goal, path, history)
    val initialInput = LabyrinthInput(visionMap, start, Direction.North, goal)
    (status, initialInput)
  }

  def processOutput(status: LabyrinthStatus, brainOutput: LabyrinthCommand.LabyrinthCommand) = Future {
    val (newPosition, newDirection) = brainOutput match {
      case LabyrinthCommand.Forward if (status.robotPosition + status.robotDirection).inLabyrinth(status.labyrinth) =>
        (status.robotPosition + status.robotDirection, status.robotDirection)
      case LabyrinthCommand.Forward =>
        (status.robotPosition, status.robotDirection)
      case LabyrinthCommand.TurnLeft =>
        (status.robotPosition, status.robotDirection.turnLeft)
      case LabyrinthCommand.TurnRight =>
        (status.robotPosition, status.robotDirection.turnRight)
    }

    val path = newPosition :: status.path
    val history = brainOutput :: status.history
    val obs = vision(status.labyrinth, newPosition)
    obs.impose(status.visionMap)
    val newStatus = LabyrinthStatus(status.visionMap, status.labyrinth, newPosition, newDirection, status.goal, path, history)
    val newInput = LabyrinthInput(status.visionMap, newPosition, newDirection, status.goal)

    (newStatus, if (newPosition == status.goal) None else Some(newInput))
  }

  override def scoreOutput(status: LabyrinthStatus, brainOutput: LabyrinthCommand.LabyrinthCommand) = Future { LabyrinthScore(0.0) }

  override def serialize(status: LabyrinthStatus): Future[DataFlowFormat] = Future.successful {
    DataFlowFormat.empty("ROBOT", "LabyrinthInput", "LabyrinthOutput")
  }
}
