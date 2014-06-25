package geneticmachine

import akka.actor.{ ActorLogging, ActorRef, Actor}
import akka.pattern.ask
import scala.util.{Failure, Success}
import common.{InitializationProtocol, MessageProtocol}
import scala.concurrent.Future
import scala.reflect.ClassTag
import akka.util.Timeout
import scala.concurrent.duration._


object Robot {
  import MessageProtocol._

  case object Status extends Request
  case object Statistic extends Request

  case class Finish[+StatusT : ClassTag](brain: ActorRef, status: StatusT) extends Response
  case class Failure(e: Throwable, brain: ActorRef) extends Response
}

/**
 * Provides shell for correct [[geneticmachine.Brain]] activity.
 * It's the adapter from 'real' world to abstract brain space.
 *
 * Robot can't be stopped until the task is solved or an exception is raised.
 *
 * Robot is both sensor and actuator for brain.
 *
 * @param brain brain to guide. It's not a child of robot, because brain is much more valuable than
 *              it's shell and it could be used outside afterwards.
 */
abstract class Robot[InputT : ClassTag, StatusT : ClassTag,
                     OutputT : ClassTag, ActuatorResponseT : ClassTag]
  (val brain: ActorRef) extends Actor with InitializationProtocol[(StatusT, InputT)] with ActorLogging {

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
  def init(): Future[(StatusT, InputT)]

  final def initialize(statusAndInput: (StatusT, InputT)) {
    val (status, input) = statusAndInput
    context.become(scoreBrain(status))
    brain ! Brain.Input(input)
  }

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

  final def waitBrain(status: StatusT, brainResponse: OutputT): Receive = {
    case MessageProtocol.Ready if sender() == brain =>
      val state = processOutput(status, brainResponse)

      state.onSuccess {
        case (newStatus: StatusT, Some(inputData: InputT)) =>
          context.become(scoreBrain(newStatus), discardOld = true)
          brain ! Brain.Input (inputData)

        case (newStatus: StatusT, None) =>
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

  final def receive: Receive = uninitialized
}
