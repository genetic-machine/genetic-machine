package geneticmachine

import akka.actor._
import akka.pattern.{ ask, pipe }
import common.dataflow.DataFlowFormat
import scala.util.{Try, Failure, Success}
import common.MessageProtocol
import scala.concurrent.Future
import scala.reflect.ClassTag
import akka.util.Timeout
import scala.concurrent.duration._

object Robot {
  import MessageProtocol._

  case class Finish[+StateT : ClassTag](brain: ActorRef, result: RobotResult[StateT]) extends Response
}

trait RobotFactory[I, O, F, +S] extends Serializable {
  def errorDff(e: Throwable): DataFlowFormat = {
    DataFlowFormat.errorDff(DataFlowFormat.robotLabel, e)
  }

  def props(brain: ActorRef): Props
}

case class RobotResult[+StateT : ClassTag]
  (worldState: StateT,
   metrics: Map[String, Double],
   continuousMetrics: Map[String, Seq[Double]]) {
  override def toString: String = {
    (for {
      (metricName, stat) <- metrics
    } yield s"$metricName: $stat").mkString("\n") +
    (for {
      (metricName, stats) <- continuousMetrics
      statsFormatted = stats.map { s: Double =>
        "%1.0f" format s
      }
    } yield s"$metricName: ${statsFormatted.mkString(" ")}").mkString("\n")
  }
}

/**
 * Provides shell for correct [[geneticmachine.Brain]] activity.
 * It's an adapter from 'real' world to abstract brain terms.
 *
 * Robot *can't* be stopped until the task is solved or an exception is received.
 *
 * Robot is both sensor and actuator for the brain.
 *
 *@param brain brain to guide.
 */
abstract class Robot[InputT : ClassTag, StateT : ClassTag, OutputT : ClassTag, FeedbackT : ClassTag]
  (val brain: ActorRef, val metrics: List[Metric[StateT]], val continuousMetrics: List[ContinuousMetric[StateT]])
    extends Actor with ActorLogging with Stash {


  private case class WorldStateDone(state: StateT, input: Option[InputT])
  private case class FeedbackDone(feedback: FeedbackT)
  private case class Initialized(state: StateT, input: Option[InputT])

  import context.dispatcher

  implicit val timeout = Timeout(1.minute)

  /**
   * Robot's initialisation.
   * @return initial status and initial brain's input.
   */
  protected def init: Future[(StateT, Option[InputT])]

  /**
   * The "physics" of the location.
   * Processes current status and brain's output into new status and sensor data.
   *
   * @param status current status of robot and environment
   * @param brainOutput brain's response
   * @return Some(), that includes new robot's status and new brain's input, if goal is not achieved, otherwise None
   */
  protected def process(status: StateT, brainOutput: OutputT): Future[(StateT, Option[InputT])]

  /**
   * The supervisor's function.
   *
   * @param status current status of robot and environment
   * @param brainOutput brain's response
   */
  protected def feedback(status: StateT, brainOutput: OutputT): Future[FeedbackT]

  protected def serialize(status: StateT): Future[DataFlowFormat]

  private final def mixinMetrics(dff: DataFlowFormat, result: RobotResult[StateT]): DataFlowFormat = {
    val metricProps: Map[String, Any] =
      (for {
        (metricName, value: Double) <- result.metrics
      } yield (s"#$metricName", value)) ++
      (for {
        (metricName, value: Seq[Double]) <- result.continuousMetrics
      } yield (s"#$metricName", value.toArray))

    dff.copy(dff.props ++ metricProps)
  }

  private final def result(state: StateT): RobotResult[StateT] = {
    val metricsValues = metrics.par.map { m =>
      (m.metricName, m(state))
    }.seq.toMap

    val continuousMetricsValues = continuousMetrics.par.map { m =>
      (m.metricName, m(state))
    }.seq.toMap

    RobotResult(state, metricsValues, continuousMetricsValues)
  }

  private final def resettingBrain(finalState: StateT): Receive = {
    case MessageProtocol.Ready if context.sender() == brain =>
      val brainResult = result(finalState)

      context.become {
        finished(brainResult)
      }

      context.parent ! Robot.Finish(brain, brainResult)
  }

  private final def processingWorldState: Receive = {
    case WorldStateDone(state: StateT, Some(input: InputT)) =>
      context.become {
        waitingOutput(state) orElse
          failureReceive("waiting output", classOf[Brain.Output[OutputT]])
      }

      log.debug(s"World state:\n$state")
      brain ! Brain.Input(input)

    case WorldStateDone(state: StateT, None) =>
      context.become {
        resettingBrain(state) orElse
          failureReceive("resetting brain", MessageProtocol.Ready)
      }

      brain ! Brain.Reset
  }

  private final def waitingAck(state: StateT, output: OutputT): Receive = {
    case MessageProtocol.Ready if context.sender() == brain =>
      context.become {
        processingWorldState orElse
          failureReceive("processing world state", classOf[WorldStateDone])
      }

      (for {
        (updatedState, inputData) <- process(state, output)
      } yield WorldStateDone(updatedState, inputData)) pipeTo self
  }

  private final def scoring(state: StateT, output: OutputT): Receive = {
    case FeedbackDone(feedbackData: FeedbackT) =>

      context.become {
        waitingAck(state, output)
      }

      brain ! Brain.Feedback(feedbackData)
  }

  /**
   * Processes brain outputs.
   * @param state current status of robot and environment.
   */
  private final def waitingOutput(state: StateT): Receive = {
    case Brain.Output(brainOutput: OutputT) =>

      context.become {
        scoring(state, brainOutput) orElse
          failureReceive("scoring brain", classOf[FeedbackDone])
      }

      (for {
        actionFeedback <- feedback(state, brainOutput)
      } yield FeedbackDone(actionFeedback)) pipeTo self
  }

  private final def finished(result: RobotResult[StateT]): Receive = {
    case MessageProtocol.Serialize =>
      (for {
        dff <- serialize(result.worldState)
      } yield {
        MessageProtocol.Serialized(mixinMetrics(dff, result))
      }) recover {
        case e: Throwable =>
          MessageProtocol.Fail(e)
      } pipeTo context.sender()
  }

  private final def failed(activity: String)(e: Throwable): Receive = {
    case msg =>
      context.sender() ! MessageProtocol.Fail {
        new Exception(s"Robot has failed during $activity!", e)
      }
  }

  private final def failureReceive(activity: String, expecting: Any): Receive = {
    case akka.actor.Status.Failure(e: Throwable) =>
      log.error(s"Main activity ($activity) has been failed! [$e]")
      failure(e)
      context.become(failed(activity)(e))

    case msg =>
      val e = MessageProtocol.UnexpectedResponse(s"Unexpected message during $activity", msg, expecting)
      log.error(s"Main activity ($activity) has been failed by unexpected message! [$e]")
      failure(e)
      context.become(failed(activity)(e))
  }

  private final def failure(e: Throwable) {
    context.parent ! MessageProtocol.Fail(e)
  }

  final val receive: Receive = {
    processingWorldState orElse failureReceive("initialization", classOf[WorldStateDone])
  }

  private final def initialization() {
    (for {
      (initialState, initialInput) <- init
    } yield WorldStateDone(initialState, initialInput)) pipeTo self
  }

  initialization()
}
