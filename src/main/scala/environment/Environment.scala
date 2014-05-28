package environment

import akka.actor.{Props, ActorLogging, Actor, ActorRef}
import common.MessageProtocol
import scala.concurrent.Future
import scala.util.{Success, Failure}

object Environment extends MessageProtocol {
  case class GenLocation[+LocationParams](params: LocationParams) extends Request

  case class LocationGenDone(locationNode: ActorRef) extends Response
  case object LocationGenFail extends Response
}

abstract class Environment[+LocationParams, +Location] (val locationNodeProps: Location => Props)
    extends Actor with ActorLogging {

  import Environment._
  import context.dispatcher

  def generateLocation(params: LocationParams): Location

  def genLocationRequest: Receive = {
    case GenLocation(params: LocationParams) =>
      val requester = sender()

      Future {
        generateLocation(params)
      }.onComplete {
        case Failure(e) =>
          log.error(e, "Map generation fail!")
          requester ! LocationGenFail

        case Success(generatedLocation) =>
          val mapNode = context.actorOf(locationNodeProps(generatedLocation), "mapNode")
          requester ! LocationGenDone(mapNode)
      }
  }

  def receive: Receive = genLocationRequest
}
