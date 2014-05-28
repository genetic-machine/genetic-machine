package environment

import akka.actor.{Props, ActorRef, Actor}
import common.MessageProtocol
import breeze.linalg.DenseMatrix

object LocationNode extends MessageProtocol {
  case object GetLocationParams extends Request
  case class Observe[+ObservationParams](params: ObservationParams) extends Request

  case class LocationParams[+LocationParams](params: LocationParams) extends Response
  case class ObservationData[+Observation, +ObservationParams](data: Observation,
                                                               from: ObservationParams) extends Response
}

abstract class LocationNode[+Location, +LocationParams,
                            +Observation, +ObservationParams](val location: Location) extends Actor {
  import LocationNode._

  def observation(from: ObservationParams): Observation
  def locationParams: LocationParams

  val props: Location => Props = { location: Location =>
    Props(this.getClass, location)
  }

  def guideRobot: Receive = {
    case LocationParams =>
      sender() ! LocationParams(locationParams)

    case Observe(from: ObservationParams) =>
      val data = observation(from)
      sender ! ObservationData(data, from)
  }

  def receive: Receive = guideRobot
}
