package robot

import akka.actor.{ActorRef, Actor}
import common.MessageProtocol

object Brain extends MessageProtocol {
  case object Reset extends Request
  case class Input[+InputT](data: InputT) extends Request
  case class Sensor(sensor: ActorRef) extends Request
  case class Actuator(actuator: ActorRef) extends Request

  case class Output[+OutputT](data: OutputT) extends Response
}

abstract class Brain[InputT, OutputT] extends Actor {

  import Brain._

  def doIntelligence(sensor: ActorRef, actuator: ActorRef): Receive

  def waitActuator(sensor: ActorRef): Receive = {
    case Actuator(actuator) =>
      sender ! Ready
      context.become(doIntelligence(sensor, actuator))
    case Reset =>
      context.become(waitSensor)
  }

  def waitSensor: Receive = {
    case Sensor(sensor) =>
      sender ! Done
      context.become(waitActuator(sensor))
    case Reset =>

  }

  def receive = waitSensor
}
