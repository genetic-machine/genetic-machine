package geneticmachine.db

import akka.actor.{Actor, ActorLogging}
import common.{MessageProtocol => MP}
import geneticmachine.db.drivers.DBDriver
import geneticmachine.ubf._

import scala.concurrent.Future

object DBActor {
  import common.MessageProtocol._

  case class Load(brainId: Long) extends Request
  case class Save(ubf: UnifiedBrainFormat) extends Request

  case class Loaded(ubf: UnifiedBrainFormat) extends Response
  case class Saved(brainId: Long) extends Response
}

abstract class DBActor[D <: DBDriver]
  extends Actor with ActorLogging {

  import DBActor._
  import context.dispatcher

  val driver: D

  final def receive: Receive = {
    case Save(ubf) =>
      val requester = context.sender()
      val request = Future { driver.saveBrain(ubf) }

      request.onSuccess {
        case id: Long =>
          requester ! Saved(id)
      }

      request.onFailure {
        case e: Throwable =>
          log.error(e, "Driver failed!")
          requester ! MP.Fail(e)
      }

    case Load(id: Long) =>
      val requester = context.sender()
      val request = Future { driver.loadBrain(id) }

      request.onSuccess {
        case ubf =>
          requester ! Loaded(ubf)
      }

      request.onFailure {
        case e: Throwable =>
          log.error(e, "Driver failed!")
      }

    case _ =>
  }

  override def postStop() {
    driver.shutdown()
  }
}
