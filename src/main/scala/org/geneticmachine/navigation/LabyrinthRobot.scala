package org.geneticmachine.navigation

/*import org.geneticmachine.navigation.utils.LabyrinthInfo
import org.geneticmachine.machine.RobotFactory
import org.geneticmachine.{RobotActor$, Metric, ContinuousMetric}

import akka.actor.{Props, ActorRef}

import common.dataflow.{DataFlowFormatBuilder, DataFlowFormat}
import common.dataflow.DataFlowFormat._

import scala.concurrent.Future
import org.geneticmachine.navigation.vision._
import org.geneticmachine.navigation.generators._
import org.geneticmachine.navigation.feedback._

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
                                (feedbackStrategy: FeedbackStrategyGenerator[FeedbackStrategy])
                                (metrics: List[Metric[LabyrinthState]])
                                (continuousMetrics: List[ContinuousMetric[LabyrinthState]])
  extends RobotFactory[LabyrinthInput, LabyrinthOutput, LabyrinthFeedback, LabyrinthState] {

  def props(brain: ActorRef): Props = Props(classOf[LabyrinthRobot], brain, labGen, vision, feedbackStrategy, metrics, continuousMetrics)

  override def toString: String = s"LabyrinthRobot($labGen, $vision, $feedbackStrategy)"
}

class LabyrinthRobot(brain: ActorRef, labyrinthGen: LabyrinthGenerator,
                     val vision: Vision, feedbackStrategyGen: FeedbackStrategyGenerator[FeedbackStrategy],
                     metrics: List[Metric[LabyrinthState]],
                     continuousMetrics: List[ContinuousMetric[LabyrinthState]])
  extends RobotActor[LabyrinthInput, LabyrinthState, LabyrinthCommand.LabyrinthCommand, LabyrinthFeedback](brain, metrics, continuousMetrics) {

  import context.dispatcher

  var feedbackStrategy: FeedbackStrategy = null // NULLL!!!!

  override def init = Future {
    val (lab, start, goal) = labyrinthGen()

    val obs = vision(lab, start)
    val visionMap = obs.impose(Labyrinth.unknown(lab.rows, lab.cols))

    val path = List(start)
    val history = List()


    val state = LabyrinthState(visionMap, lab, start, goal, path, history)
    val initialInput = LabyrinthInput(visionMap, start, goal)

    this.feedbackStrategy = feedbackStrategyGen(state)

    (state, Some(initialInput))
  }

  def process(state: LabyrinthState, brainOutput: LabyrinthCommand.LabyrinthCommand) = Future {
    val newPosition = state.robotPosition.action(state.labyrinth)(brainOutput)

    val path = newPosition :: state.path
    val history = brainOutput :: state.history
    val obs = vision(state.labyrinth, newPosition)

    obs.impose(state.visionMap)

    val newStatus = LabyrinthState(state.visionMap, state.labyrinth, newPosition, state.goal, path, history)
    val newInput = LabyrinthInput(state.visionMap, newPosition, state.goal)

    (newStatus, if (newPosition.point == state.goal) { None } else { Some(newInput) })
  }

  override def feedback(state: LabyrinthState, brainOutput: LabyrinthCommand.LabyrinthCommand) = Future {
    if (state.history.size % 1 == 0) {
      log.info(s"\n${LabyrinthInfo.formatLabyrinthState(state)}")
    }

    LabyrinthFeedback(feedbackStrategy(state, brainOutput))
  }

  override def serialize(status: LabyrinthState): Future[DataFlowFormat] = Future.successful {
    val builder = DataFlowFormatBuilder(robotLabel).withType("Labyrinth Robot")

    val input = builder.node("RobotInput").asInput()
    val output = builder.node("BrainScore").asOutput()

    val robotNode = builder.node("Experiment")
    robotNode("Labyrinth generator" -> labyrinthGen.toString)
    robotNode("Vision" -> vision.toString)
    robotNode("Feedback" -> feedbackStrategy.toString)

    val (labRepr, rows, cols) = Labyrinth.toArray(status.labyrinth)
    robotNode("Labyrinth" -> labRepr)
    robotNode("Rows" -> rows)
    robotNode("Cols" -> cols)
    robotNode("Commands" -> status.history.map { _.id }.toArray)

    robotNode("TrajectoryX" -> status.path.map { _.point.x }.toArray)
    robotNode("TrajectoryY" -> status.path.map { _.point.y }.toArray)

    robotNode("TrajectoryDir" -> status.path.map { rp => Direction.id(rp.direction) }.toArray)

    input --> robotNode
    robotNode --> output

    builder.toDataFlowFormat
  }
}
*/