package robot

import akka.actor.{ActorLogging, ActorRef, Actor}
import akka.pattern.ask
import common.MessageProtocol
import scala.util.{Failure, Success}

object Robot extends MessageProtocol {
  case class Location(location: ActorRef) extends Request
  case object Status extends Request

  case class StatusResponse[+StatusT](status: StatusT) extends Response
  case class Finish(brain: ActorRef) extends Response
}

/**
 * [[robot.Robot]] provides shell for correct [[robot.Brain]] activity.
 * It's like an adapter from 'real' world to abstract brain space.
 *
 * @param brain brain to guide. It's not a child of robot, because brain is much more valuable than
 *              it's shell and it could be used outside, for example, in multi-robot.
 */
abstract class Robot[+InputT, +OutputT, +StatusT,
                     +ObservationT, +ObservationParams,
                     +LocationParams] (val brain: ActorRef)
  extends Actor with ActorLogging {

  import Robot._
  import environment.LocationNode._
  import context.dispatcher

  def changeStatus(command: OutputT): Option[ObservationParams]
  def applyObservation(data: ObservationT, from: ObservationParams): InputT
  def currentStatus: StatusT
  def statusReset(params: LocationParams): ObservationParams

  def observe(from: ObservationParams, locationNode: ActorRef) {
    (locationNode ? Observe(from)).onComplete {
      case Success(ObservationData(data: ObservationT, from: ObservationParams)) =>
        val input = applyObservation(data, from)
        brain ! Brain.Input(input)

      case Success(_) =>
        failure(MissType(classOf[ObservationT]), "Observation type")

      case Failure(e) =>
        failure(e, "Can't observe location!")
    }
  }

  def failure(e: Throwable, message: String) {
    log.error(e, message)
    brain ! Brain.Reset
    context.stop(self)
  }

  def guideBrain(locationNode: ActorRef): Receive = {
    case Brain.Output(data: OutputT) =>
      changeStatus(data) match {
        case None =>
          brain ! Brain.Reset
          context.parent ! Finish(brain)
          context.unbecome()

        case Some(from) =>
          observe(from, locationNode)
      }

    case Brain.Output(msg) =>
      failure(MissType(msg.getClass), "Brain")
  }

  def locationReceive: Receive = {
    case Location(locationNode) =>
      val locationParams = locationNode ? GetLocationParams

      locationParams.onSuccess {
        case LocationParams(params: LocationParams) =>
          val from = statusReset(params)
          observe(from, locationNode)

        case other =>
          failure(MissType(other.getClass), "Location params")
      }

      locationParams.onFailure {
        case e =>
          failure(e, "Can't receive location params!")
      }

      context.become(guideBrain(locationNode) orElse statusResponse, discardOld = false)
  }

  def statusResponse: Receive = {
    case Status =>
      sender() ! currentStatus
    case other =>
      log.error(s"Unhandled message: $other")
  }

  def receive: Receive = locationReceive orElse statusResponse
}
