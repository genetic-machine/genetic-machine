package org.geneticmachine.navigation

import org.geneticmachine._
import org.geneticmachine.common.graph.{GraphBuilder, Graph}

import scala.concurrent.Future
import org.geneticmachine.navigation.vision._
import org.geneticmachine.navigation.generators._
import org.geneticmachine.navigation.feedback._

case class NavigationEnvironmentGen(labGen: LabyrinthGenerator)
                                    (vision: Vision)
                                    (feedbackStrategy: FeedbackStrategyGenerator)
                                    (metrics: List[Metric[NavigationState]])
                                    (continuousMetrics: List[ContinuousMetric[NavigationState]])
  extends EnvironmentGen[NavigationInput, NavigationOutput, NavigationState, ExecutionContext] {

  override def apply(c: ExecutionContext): NavigationEnvironment = {
    new NavigationEnvironment(c)(labGen, vision, feedbackStrategy, metrics, continuousMetrics)
  }

  override def toString: String = s"Navigation environment [$labGen, $vision, $feedbackStrategy]"
}

class NavigationEnvironment(protected val context: ExecutionContext)
                           (labyrinthGen: LabyrinthGenerator, val vision: Vision,
                            feedbackStrategyGen: FeedbackStrategyGenerator,
                            override val metrics: List[Metric[NavigationState]],
                            override val continuousMetrics: List[ContinuousMetric[NavigationState]])
  extends Environment[NavigationInput, NavigationOutput, NavigationState] {

  import context.futureExecutionContext

  var feedbackStrategy: FeedbackStrategy = null // NULLL!!!!

  override def init: Future[(NavigationState, Option[NavigationInput])] = Future {
    val (lab, start, goal) = labyrinthGen()

    val obs = vision(lab, start)
    val visionMap = obs.impose(Labyrinth.unknown(lab.rows, lab.cols))

    val path = List(start)
    val history = List()


    val state = NavigationState(visionMap, lab, start, goal, path, history)
    val initialInput = NavigationInput(visionMap, start, goal, 0.0)

    feedbackStrategy = feedbackStrategyGen(state)

    (state, Some(initialInput))
  }

  def process(state: NavigationState, algorithmAction: NavigationCommand) = Future {
    context.logger.info {
      s"\n${utils.NavigationInfo.formatNavigationState(state)}"
    }

    val newPosition = state.robotPosition.action(state.labyrinth)(algorithmAction)

    val path = newPosition :: state.path
    val history = algorithmAction :: state.history
    val obs = vision(state.labyrinth, newPosition)

    obs.impose(state.visionMap)

    val newStatus = NavigationState(state.visionMap, state.labyrinth, newPosition, state.goal, path, history)
    val feedback: Double = feedbackStrategy(state, algorithmAction)
    val newInput = NavigationInput(state.visionMap, newPosition, state.goal, feedback)

    (newStatus, if (newPosition.point == state.goal) { None } else { Some(newInput) })
  }

  override def serialize(status: NavigationState): Future[Graph] = Future.successful {
    val builder = GraphBuilder(Graph.environmentLabel).withType("Navigation environment")

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
    robotNode("Commands" -> status.history.toArray)

    robotNode("TrajectoryX" -> status.path.map { _.point.x }.toArray)
    robotNode("TrajectoryY" -> status.path.map { _.point.y }.toArray)

    robotNode("TrajectoryDir" -> status.path.map { rp => Direction.char(rp.direction) }.toArray)

    input --> robotNode
    robotNode --> output

    builder.toGraph
  }
}