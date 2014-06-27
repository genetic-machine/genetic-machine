package geneticmachine

import akka.actor.{ ActorLogging, ActorRef, Actor}
import akka.pattern.ask
import geneticmachine.dataflow.DataFlowFormat
import scala.util.{Failure, Success}
import common.MessageProtocol
import scala.concurrent.Future
import scala.reflect.ClassTag
import akka.util.Timeout
import scala.concurrent.duration._


object Robot {
  import MessageProtocol._

  case class Finish[+StatusT : ClassTag](brain: ActorRef, status: StatusT) extends Response
  case class Failure(e: Throwable, brain: ActorRef) extends Response
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
                     OutputT : ClassTag, ActuatorResponseT : ClassTag]
  (val brain: ActorRef) extends Actor with ActorLogging {

  import context.dispatcher

  implicit val timeout = Timeout(1.minute)

  def unexpectedResponse(reason: String, msg: Any) {
    val e = MessageProtocol.UnexpectedResponse(reason, msg)
    log.error(e, s"Unexpected response! $msg")
    failure(e)
  }

  def failure(e: Throwable) {
    context.parent ! Robot.Failure(e, brain)
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
  def processOutput(status: StatusT, brainOutput: OutputT): Future[(StatusT, Option[InputT])]

  /**
   * The supervisor's function.
   *
   * @param status current status of robot and environment
   * @param brainOutput brain's response
   */
  def scoreOutput(status: StatusT, brainOutput: OutputT): Future[ActuatorResponseT]

  def serialize(status: StatusT): Future[DataFlowFormat]

  final def waitBrain(status: StatusT, brainResponse: OutputT): Receive = {
    case MessageProtocol.Ready if sender() == brain =>
      val state = processOutput(status, brainResponse)

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
              context.parent ! Robot.Failure(e, brain)

            case msg =>
              context.parent ! Robot.Failure(MessageProtocol.UnexpectedResponse("brain release", msg), brain)
          }
      }

      state.onFailure {
        case e => failure(e)
      }

    case msg if context.sender() == brain =>
      unexpectedResponse(s"must be ${MessageProtocol.Ready} instead of $msg", msg)

    case _ =>
      context.sender() ! MessageProtocol.Busy
  }

  /**
   * Processes brain outputs.
   * @param status current status of robot and environment.
   */
  final def scoreBrain(status: StatusT): Receive = {
    case Brain.Output(brainOutput: OutputT) =>
      val score = scoreOutput(status, brainOutput)

      score.onSuccess {
        case response: ActuatorResponseT =>
          context.become(waitBrain(status, brainOutput), discardOld = true)
          brain ! Brain.Feedback(response)
      }

      score.onFailure {
        case e =>
          context.become(waitBrain(status, brainOutput), discardOld = true)
          failure(e)
      }

    case msg if sender() == brain =>
      unexpectedResponse(s"must be ${classOf[Brain.Output[OutputT]]}", msg)
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
