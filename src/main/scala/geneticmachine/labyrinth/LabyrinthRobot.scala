package geneticmachine.labyrinth

import geneticmachine.{RobotFactory, Robot, Metric, ContinuousMetric}

import akka.actor.{Props, ActorRef}

import common.dataflow.{DataFlowFormatBuilder, DataFlowFormat}
import common.dataflow.DataFlowFormat._

import scala.concurrent.Future
import geneticmachine.labyrinth.vision._
import geneticmachine.labyrinth.generators._
import geneticmachine.labyrinth.feedback._

object LabyrinthRobot {
  def sampleFactory: LabyrinthRobotFactory = {
    val labGen = RandomWalkGenerator(3, 3)(Point(51, 51))
    val vision = SimpleVision(5)
    val feedback = ZeroFeedback
    val ms = List(metrics.CommandNumber)
    val cms = List(metrics.ManhattanDistanceToTarget, metrics.EuclideanDistanceToTarget)
    LabyrinthRobotFactory(labGen)(vision)(feedback)(ms)(cms)
  }
}

case class LabyrinthRobotFactory(labGen: LabyrinthGenerator)
                                (vision: Vision)
                                (feedbackStrategy: FeedbackStrategy)
                                (metrics: List[Metric[LabyrinthState]])
                                (continuousMetrics: List[ContinuousMetric[LabyrinthState]])
  extends RobotFactory[LabyrinthInput, LabyrinthOutput, LabyrinthFeedback, LabyrinthState] {

  def props(brain: ActorRef) = Props(classOf[LabyrinthRobot], brain, labGen, vision, feedbackStrategy, metrics, continuousMetrics)

  override def toString: String = s"LabyrinthRobot($labGen, $vision, $feedbackStrategy)"
}

class LabyrinthRobot(brain: ActorRef, val labyrinthGen: LabyrinthGenerator,
                     val vision: Vision, val feedbackStrategy: FeedbackStrategy,
                     metrics: List[Metric[LabyrinthState]],
                     continuousMetrics: List[ContinuousMetric[LabyrinthState]])
  extends Robot[LabyrinthInput, LabyrinthState, LabyrinthCommand.LabyrinthCommand, LabyrinthFeedback](brain, metrics, continuousMetrics) {

  import context.dispatcher

  override def init = Future.successful {
    val (lab, start, goal) = labyrinthGen()

    val obs = vision(lab, start)
    val visionMap = obs.impose(Labyrinth.unknown(lab.rows, lab.cols))

    val path = List(start)
    val history = List()

    val status = LabyrinthState(visionMap, lab, start, Direction.North, goal, path, history)
    val initialInput = LabyrinthInput(visionMap, start, Direction.North, goal)
    (status, Some(initialInput))
  }

  def process(status: LabyrinthState, brainOutput: LabyrinthCommand.LabyrinthCommand) = Future {
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
    val newStatus = LabyrinthState(status.visionMap, status.labyrinth, newPosition, newDirection, status.goal, path, history)
    val newInput = LabyrinthInput(status.visionMap, newPosition, newDirection, status.goal)

    (newStatus, if (newPosition == status.goal) None else Some(newInput))
  }

  override def feedback(status: LabyrinthState, brainOutput: LabyrinthCommand.LabyrinthCommand) = Future {
    feedbackStrategy(status, brainOutput)
  }

  override def serialize(status: LabyrinthState): Future[DataFlowFormat] = Future.successful {
    val builder = DataFlowFormatBuilder(robotLabel)

    val input = builder.node("RobotInput").asInput()
    val output = builder.node("BrainScore").asOutput()

    val robotNode = builder.node("Experiment")
    robotNode("Labyrinth generator" -> labyrinthGen.toString)
    robotNode("Vision" -> vision.toString)
    robotNode("Learning" -> feedbackStrategy.toString)

    val (labRepr, rows, cols) = Labyrinth.toArray(status.labyrinth)
    robotNode("Labyrinth" -> labRepr)
    robotNode("Rows" -> rows)
    robotNode("Cols" -> cols)
    robotNode("Commands" -> status.history.map { _.id }.toArray)
    robotNode("TrajectoryX" -> status.path.map { _.x }.toArray)
    robotNode("TrajectoryY" -> status.path.map { _.y }.toArray)

    input --> robotNode
    robotNode --> output

    builder.toDataFlowFormat
  }
}
