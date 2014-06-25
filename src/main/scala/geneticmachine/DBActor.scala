package geneticmachine

import geneticmachine.ubf._
import common.{MessageProtocol => MP, InitializationProtocol}

import akka.actor.{ActorLogging, Actor}
import akka.pattern.pipe

object DBActor {
  import MP._

  case class Load(brainId: Long) extends Request
  case class Save(ubf: UnifiedBrainFormat) extends Request

  case class Loaded(ubf: UnifiedBrainFormat) extends Response
  case class Saved(brainId: Long) extends Response
}

abstract class DBActor[D <: UnifiedBrainFormatDriver](val dbPath: String)
  extends Actor with InitializationProtocol[D] with ActorLogging {

  import geneticmachine.DBActor._
  import context.dispatcher

  def initialized(driver: D): Receive = {
    case Save(ubf: UnifiedBrainFormat) =>
      (for {
        id <- driver.save(ubf)
      } yield Saved(id)) pipeTo context.sender()

    case Load(id: Long) =>
      (for {
        ubf <- driver.load(id)
      } yield Loaded(ubf)) pipeTo context.sender()
  }

  override def initialize(driver: D) {
    context.become(initialized(driver), discardOld = true)
  }


  override def receive: Receive = uninitialized
}
