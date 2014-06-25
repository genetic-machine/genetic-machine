package common

import akka.actor.Actor

import scala.concurrent.Future

trait InitializationProtocol[S] extends Actor {
  def init(): Future[S]

  def initialize(status: S): Unit

  import context.dispatcher

  def uninitialized: Receive = {
    case MessageProtocol.Init =>
      val initialization = init()
      val requester = context.sender()
      initialization.onSuccess {
        case s =>
          initialize(s)
          requester ! MessageProtocol.Ready
      }

      initialization.onFailure {
        case e: Throwable =>
          requester ! MessageProtocol.Fail(MessageProtocol.InitializationFailure(e))
          context.stop(self)
      }
  }
}
