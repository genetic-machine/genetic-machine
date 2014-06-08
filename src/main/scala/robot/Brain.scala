package robot

import akka.actor.{ActorLogging, ActorRef, Actor}
import common.MessageProtocol
import scala.reflect.ClassTag

object Brain {

  import MessageProtocol._

  case object Reset extends Request
  case class Input[+InputT](data: InputT) extends Request
  case class Sensor(sensor: ActorRef) extends Request
  case class Actuator(actuator: ActorRef) extends Request

  case class Output[+OutputT](data: OutputT) extends Response
}

abstract class Brain[InputT : ClassTag, OutputT : ClassTag] extends Actor with ActorLogging {

  import Brain._

  def doIntelligence(sensor: ActorRef, actuator: ActorRef): Receive

  def waitActuator(sensor: ActorRef): Receive = {
    case Actuator(actuator) =>
      sender ! MessageProtocol.Ready
      context.become(doIntelligence(sensor, actuator))
    case Reset =>
      context.become(waitSensor)
  }

  def waitSensor: Receive = {
    case Sensor(sensor) =>
      sender ! MessageProtocol.Done
      context.become(waitActuator(sensor))
    case Reset =>

  }

  def receive = waitSensor
}
