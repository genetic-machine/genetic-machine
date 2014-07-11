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

  case class Finish[+StatusT : ClassTag](brain: ActorRef, status: StatusT) extends Response
}

trait RobotFactory[I, O, F, +S] extends Serializable {
  def errorDff(e: Throwable): DataFlowFormat = {
    DataFlowFormat.errorDff(DataFlowFormat.robotLabel, e)
  }

  def props(brain: ActorRef): Props
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
abstract class Robot[InputT : ClassTag, StatusT : ClassTag,
                     OutputT : ClassTag, FeedbackT : ClassTag]
  (val brain: ActorRef) extends Actor with ActorLogging {

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
  def init: (StatusT, InputT)

  /**
   * The "physics" of the location.
   * Processes current status and brain's output into new status and sensor data.
   *
   * @param status current status of robot and environment
   * @param brainOutput brain's response
   * @return Some(), that includes new robot's status and new brain's input, if goal is not achieved, otherwise None
   */
  def process(status: StatusT, brainOutput: OutputT): Future[(StatusT, Option[InputT])]

  /**
   * The supervisor's function.
   *
   * @param status current status of robot and environment
   * @param brainOutput brain's response
   */
  def feedback(status: StatusT, brainOutput: OutputT): Future[FeedbackT]

  def serialize(status: StatusT): Future[DataFlowFormat]

  final def waitBrain(status: StatusT, brainResponse: OutputT): Receive = {
    case MessageProtocol.Ready if sender() == brain =>
      val state = process(status, brainResponse)

      state.onSuccess {
        case (newStatus: StatusT, Some(inputData: InputT)) =>
          context.become(scoreBrain(newStatus), discardOld = true)
          brain ! Brain.Input (inputData)

        case (newStatus: StatusT, None) =>
          context.become(finish(newStatus))

          (brain ? Brain.Reset).onComplete {
            case Success(MessageProtocol.Ready) =>
              context.parent ! Robot.Finish(brain, newStatus)

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
  final def scoreBrain(status: StatusT): Receive = {
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

  final def finish(status: StatusT): Receive = {
    case MessageProtocol.Serialize =>
      val requester = context.sender()
      val serialization = serialize(status)

      serialization.onSuccess {
        case dff =>
          requester ! MessageProtocol.Serialized(dff)
      }

      serialization.onFailure {
        case e: Throwable =>
          requester ! MessageProtocol.Fail(e)
      }
  }

  final val receive: Receive = {
    val (status, input) = init
    brain ! Brain.Input(input)
    scoreBrain(status)
  }
}
