package robot

import akka.actor.{ActorRef, Actor}
import common.MessageProtocol

object Brain extends MessageProtocol {
  case object Reset extends Request
  case class Input[+InputT](data: InputT) extends Request

  case class Output[+OutputT](data: OutputT) extends Response
}

abstract class Brain[InputT, OutputT](val sensor: ActorRef,
                                      val actuator: ActorRef)
  extends Actor {
}
