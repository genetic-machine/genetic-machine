package robot

import akka.actor.{ActorLogging, ActorRef, Actor}
import common.MessageProtocol
import scala.concurrent.Future
import scala.reflect.ClassTag
import scala.util.{Failure, Success}

object Brain {

  import MessageProtocol._

  case class Bind(sensor: ActorRef, actuator: ActorRef) extends Request
  case class Input[+InputT](data: InputT) extends Request
  case object Unbind extends Request
  case class CopyTo(other: ActorRef) extends Request

  case class Output[+OutputT](data: OutputT) extends Response
}

abstract class Brain[InputT : ClassTag, OutputT : ClassTag] extends Actor with ActorLogging {

  import context.dispatcher
  
  def think(data: InputT): Future[OutputT]
  def reset(): Future[Unit]

  final def busy: Receive = {
    case _ =>
      context.sender ! MessageProtocol.Busy
  }

  final def bound(sensor: ActorRef, actuator: ActorRef): Receive = {
    case Brain.Unbind =>
      val requester = context.sender()
      context.become(busy, discardOld = false)

      reset().onComplete {
        case Success(_) =>
          context.unbecome() // from busy to bound
          context.unbecome() // from bound to unbound
          requester ! MessageProtocol.Ready

        case Failure(e) =>
          context.unbecome() // from busy to bound
          requester ! MessageProtocol.Fail(e)
          context.parent ! MessageProtocol.Fail(e)
      }

    case Brain.Input(data: InputT) if sender() == sensor =>
      context.become(busy, discardOld = false)

      think(data).onComplete {
        case Success(data: OutputT) =>
          context.unbecome()
          actuator ! Brain.Output(data)

        case Failure(e) =>
          context.unbecome()
          sensor ! MessageProtocol.Fail(e)
          context.parent ! MessageProtocol.Fail(e)
      }

    case _ =>
      context.sender ! MessageProtocol.Busy
  }

  def copyTo(other: ActorRef): Future[Unit]

  final def unbound: Receive = {
    case Brain.Bind(sensor, actuator) =>
      context.become(bound(sensor, actuator), discardOld = false)
      context.sender ! MessageProtocol.Ready

    case Brain.Unbind =>
      context.sender ! MessageProtocol.Ready

    case Brain.CopyTo(other) =>
      val requester = context.sender()
      context.become(busy, discardOld = false)

      copyTo(other).onComplete {
        case Success(_) =>
          context.unbecome()
          requester ! MessageProtocol.Ready

        case Failure(e) =>
          context.unbecome()
          requester ! MessageProtocol.Fail(e)
      }
  }

  final def receive = unbound
}
