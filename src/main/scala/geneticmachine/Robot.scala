package geneticmachine

import akka.actor.{Props, ActorLogging, ActorRef, Actor}
import akka.pattern.ask
import common.dataflow.DataFlowFormat
import scala.util.{Failure, Success}
import common.MessageProtocol
import scala.concurrent.Future
import scala.reflect.ClassTag
import akka.util.Timeout
import scala.concurrent.duration._

object Robot {
  import MessageProtocol._

  case class Finish[+StatusT : ClassTag](brain: ActorRef, result: RobotResult[StatusT]) extends Response
}

trait RobotFactory[I, O, F, +S] extends Serializable {
  def errorDff(e: Throwable): DataFlowFormat = {
    DataFlowFormat.errorDff(DataFlowFormat.robotLabel, e)
  }

  def props(brain: ActorRef): Props
}

case class RobotResult[+StateT : ClassTag](worldState: StateT,
                                           metrics: Map[String, Double],
                                           continuousMetrics: Map[String, Seq[Double]])

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
  (val brain: ActorRef, val metrics: List[Metric[StateT]],
   val continuousMetrics: List[ContinuousMetric[StateT]])
    extends Actor with ActorLogging {

  import context.dispatcher

  implicit val timeout = Timeout(1.minute)

  def unexpectedResponse(during: String, msg: Any, expected: Any) {
    val e = MessageProtocol.UnexpectedResponse(during, msg, expected)
    failure(e)
  }

  def unexpectedResponse[T](during: String, msg: Any)(implicit ev: ClassTag[T]) {
    unexpectedResponse(during, msg, ev.toString())
  }

  def failure(e: Throwable) {
    log.error(e, "Robot failed!")
    context.parent ! MessageProtocol.Fail(e)
  }

  /**
   * Robot's initialisation.
   * @return initial status and initial brain's input.
   */
  def init: Future[(StateT, InputT)]

  /**
   * The "physics" of the location.
   * Processes current status and brain's output into new status and sensor data.
   *
   * @param status current status of robot and environment
   * @param brainOutput brain's response
   * @return Some(), that includes new robot's status and new brain's input, if goal is not achieved, otherwise None
   */
  def process(status: StateT, brainOutput: OutputT): Future[(StateT, Option[InputT])]

  /**
   * The supervisor's function.
   *
   * @param status current status of robot and environment
   * @param brainOutput brain's response
   */
  def feedback(status: StateT, brainOutput: OutputT): Future[FeedbackT]

  def serialize(status: StateT): Future[DataFlowFormat]

  def addMetrics(dff: DataFlowFormat, result: RobotResult[StateT]): DataFlowFormat = {
    val metricProps: Map[String, Any] =
      (for {
        (metricName, value: Double) <- result.metrics
      } yield (s"#$metricName", value)) ++
      (for {
        (metricName, value: Seq[Double]) <- result.continuousMetrics
      } yield (s"#$metricName", value.toArray))

    dff.copy(dff.props ++ metricProps)
  }

  final def finalize(state: StateT): RobotResult[StateT] = {
    val metricsValues = metrics.par.map { m =>
      (m.metricName, m(state))
    }.seq.toMap

    val continuousMetricsValues = continuousMetrics.par.map { m =>
      (m.metricName, m(state))
    }.seq.toMap

    RobotResult(state, metricsValues, continuousMetricsValues)
  }

  final def waitBrain(status: StateT, brainResponse: OutputT): Receive = {
    case MessageProtocol.Ready if sender() == brain =>
      val state = process(status, brainResponse)

      state.onSuccess {
        case (newStatus: StateT, Some(inputData: InputT)) =>
          context.become(scoreBrain(newStatus), discardOld = true)
          brain ! Brain.Input (inputData)

        case (newStatus: StateT, None) =>
          val result = finalize(newStatus)
          context.become(finish(result))

          (brain ? Brain.Reset).onComplete {
            case Success(MessageProtocol.Ready) =>
              context.parent ! Robot.Finish(brain, result)

            case Failure(e) =>
              failure(e)

            case msg =>
              unexpectedResponse("Brain reset:", msg, MessageProtocol.Ready)
          }
      }

      state.onFailure {
        case e => failure(e)
      }

    case msg if context.sender() == brain =>
      unexpectedResponse(s"Feedback propagation:", msg, MessageProtocol.Ready)

    case _ =>
      context.sender() ! MessageProtocol.Busy
  }

  /**
   * Processes brain outputs.
   * @param status current status of robot and environment.
   */
  final def scoreBrain(status: StateT): Receive = {
    case Brain.Output(brainOutput: OutputT) =>
      val score = feedback(status, brainOutput)

      score.onSuccess {
        case response: FeedbackT =>
          context.become(waitBrain(status, brainOutput), discardOld = true)
          brain ! Brain.Feedback(response)
      }

      score.onFailure {
        case e =>
          context.become(waitBrain(status, brainOutput), discardOld = true)
          failure(e)
      }

    case msg if sender() == brain =>
      unexpectedResponse[OutputT](s"Action expectation", msg)
  }

  final def finish(result: RobotResult[StateT]): Receive = {
    case MessageProtocol.Serialize =>
      val requester = context.sender()
      val serialization = serialize(result.worldState)

      serialization.onSuccess {
        case dff =>
          requester ! MessageProtocol.Serialized(addMetrics(dff, result))
      }

      serialization.onFailure {
        case e: Throwable =>
          requester ! MessageProtocol.Fail(e)
      }
  }

  init.onComplete {
    case Success((state, input)) =>
      brain ! Brain.Input(input)
      context.become(scoreBrain(state))

    case Failure(e: Throwable) =>
      failure(e)
  }

  final def receive: Receive = {
    case _ =>
  }
}
