package robot

import akka.actor.{ActorLogging, ActorRef, Actor}
import akka.pattern.ask
import common.MessageProtocol
import scala.util.{Failure, Success}
import java.util.concurrent.TimeUnit

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
abstract class Robot[InputT, OutputT, StatusT,
                     ObservationT, ObservationParamsT,
                     LocationParamsT] (val brain: ActorRef)
  extends Actor with ActorLogging {

  import environment.LocationNode._
  import context.dispatcher
  import akka.util.Timeout

  import Brain._

  implicit val timeout = Timeout(1, TimeUnit.SECONDS)

  (brain ? Sensor(self)).onComplete {
    case Success(Done) =>
      (brain ? Actuator(self)).onComplete {
        case Success(Ready) =>
        case msg =>
          failure(MissType(msg.getClass), "brain actuator setup!")
      }
    case msg =>
      failure(MissType(msg.getClass), "brain sensor setup!")
  }

  def changeStatus(command: OutputT): Option[ObservationParamsT]
  def applyObservation(data: ObservationT, from: ObservationParamsT): InputT
  def currentStatus: StatusT
  def statusReset(params: LocationParamsT): ObservationParamsT

  def observe(from: ObservationParamsT, locationNode: ActorRef) {
    implicit val timeout = Timeout(1, TimeUnit.SECONDS)

    (locationNode ? Observe(from)).onComplete {
      case Success(ObservationData(data: ObservationT, from: ObservationParamsT)) =>
        val input = applyObservation(data, from)
        brain ! Brain.Input(input)

      case Success(msg) =>
        failure(MissType(msg.getClass), "Observation type")

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
          context.parent ! Robot.Finish(brain)
          context.unbecome()

        case Some(from) =>
          observe(from, locationNode)
      }

    case Brain.Output(msg) =>
      failure(MissType(msg.getClass), "Brain output")
  }

  def locationReceive: Receive = {
    case Robot.Location(locationNode) =>

      implicit val timeout = Timeout(1, TimeUnit.SECONDS)
      val locationParams = locationNode ? GetLocationParams

      locationParams.onSuccess {
        case LocationParams(params: LocationParamsT) =>
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
    case Robot.Status =>
      sender() ! currentStatus
    case other =>
      log.error(s"Unhandled message: $other")
  }

  def receive: Receive = locationReceive orElse statusResponse
}
