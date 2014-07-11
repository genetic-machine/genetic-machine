package geneticmachine.db

import akka.actor.{Actor, ActorLogging}
import common.{MessageProtocol => MP}
import geneticmachine.db.drivers.DBDriver
import common.dataflow._

import scala.concurrent.Future

object DBActor {
  import common.MessageProtocol._

  case class Load(brainId: Long) extends Request
  case class Save(dff: DataFlowFormat) extends Request

  case class Loaded(dff: DataFlowFormat) extends Response
  case class Saved(brainId: Long) extends Response
}

abstract class DBActor[D <: DBDriver]
  extends Actor with ActorLogging {

  import DBActor._
  import context.dispatcher

  val driver: D

  final def receive: Receive = {
    case Save(dff) =>
      val requester = context.sender()
      val request = Future { driver.save(dff) }

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
      val request = Future { driver.load(id) }

      request.onSuccess {
        case dff =>
          requester ! Loaded(dff)
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
