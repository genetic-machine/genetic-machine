package robot

import akka.actor.{ ActorLogging, ActorRef, Actor}
import akka.pattern.ask
import scala.util.{Failure, Success}
import common.MessageProtocol
import scala.concurrent.Future
import scala.reflect.ClassTag
import akka.util.Timeout
import scala.concurrent.duration._


object Robot {
  import MessageProtocol._

  case object Status extends Request
  case object Statistic extends Request

  case class Finish[+StatusT](brain: ActorRef, status: StatusT) extends Response
  case class Failure(e: Throwable, brain: ActorRef) extends Response
}

/**
 * Provides shell for correct [[robot.Brain]] activity.
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
                     OutputT : ClassTag] (val brain: ActorRef) extends Actor with ActorLogging {

  import context.dispatcher

  implicit val timeout = Timeout(1 minute)

  def unexpectedResponse(reason: String)(msg: Any) {
    val e = MessageProtocol.UnexpectedResponse(reason, msg)
    log.error("Unexpected response!", e)
    failure(e)
  }

  def failure(e: Throwable) {
    context.parent ! Robot.Failure(e, brain)
  }

  /**
   * Robot's initialisation.
   * @return initial status and initial brain's input.
   */
  def selfSetup(): Future[(StatusT, InputT)]

  /**
   * The "physics" of the location.
   * Processes current status and brain's output into new status and sensor data.
   * @param status current status
   * @param brainOutput brain's response
   * @return Some(), that includes new robot's status and new brain's input, if goal is not achieved, otherwise None
   */
  def processOutput(status: StatusT, brainOutput: OutputT): Future[(StatusT, Option[InputT])]

  final def busy: Receive = {
    case _ =>
      context.sender() ! MessageProtocol.Busy
  }

  /**
   * Processes brain outputs.
   * @param status
   */
  final def guideBrain(status: StatusT): Receive = {
    case Brain.Output(data: OutputT) =>

      val state = processOutput(status, data)
      context.become(busy, discardOld = true)

      state onSuccess {
        case (newStatus: StatusT, Some(inputData: InputT)) =>
          brain ! Brain.Input (inputData)
          context.become (guideBrain (newStatus), discardOld = true)

        case (newStatus: StatusT, None) =>
          (brain ? Brain.Unbind).onComplete {
            case Success(MessageProtocol.Ready) =>
              context.parent ! Robot.Finish(brain, newStatus)

            case Failure(e) =>
              context.parent ! Robot.Failure(e, brain)

            case msg =>
              context.parent ! Robot.Failure(MessageProtocol.UnexpectedResponse("brain release", msg), brain)
          }
      }

      state onFailure {
        case e => failure(e)
      }

    case msg if context.sender() == brain =>
      unexpectedResponse("brain wrong response")(msg)

    case _ =>
      context.sender() ! MessageProtocol.Busy
  }

  final val brainSetup = for {
    result <- brain ? Brain.Bind(self, self)
    if result == MessageProtocol.Ready
  } yield result

  final val generalSetup = for {
    _ <- brainSetup
    (status, input) <- selfSetup()
  } yield (status, input)

  generalSetup.onComplete {
    case Success((status, input)) =>
      context.become(guideBrain(status))
      brain ! Brain.Input(input)

    case Failure(e) =>
      failure(e)
  }

  final def receive: Receive = {
    case _ =>
      context.sender ! MessageProtocol.Busy
  }
}
