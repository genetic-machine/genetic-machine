package environment

import akka.actor.{Props, ActorRef, Actor}
import common.MessageProtocol
import breeze.linalg.DenseMatrix

object LocationNode extends MessageProtocol {
  case object GetLocationParams extends Request
  case class Observe[ObservationParams](params: ObservationParams) extends Request

  case class LocationParams[LocationParams](params: LocationParams) extends Response
  case class ObservationData[Observation, ObservationParams](data: Observation,
                                                             from: ObservationParams) extends Response
}

abstract class LocationNode[LocationT, LocationParamsT,
                            ObservationT, ObservationParamsT](val location: LocationT) extends Actor {
  import LocationNode._

  def observation(from: ObservationParamsT): ObservationT
  def locationParams: LocationParamsT

  def guideRobot: Receive = {
    case LocationParams =>
      sender() ! LocationParams(locationParams)

    case Observe(from: ObservationParamsT) =>
      val data = observation(from)
      sender ! ObservationData(data, from)
  }

  def receive: Receive = guideRobot
}
