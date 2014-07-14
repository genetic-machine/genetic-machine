package geneticmachine.machine

import akka.actor.{ActorRef, ActorLogging, Actor}
import akka.pattern.ask
import scala.concurrent.duration._

import common.ViewProtocol
import common.dataflow.DataFlowFormat
import geneticmachine.db.DBActor

object ViewActor {
  val dbTimeout = 10.seconds
  val traverseRelations = Seq(DataFlowFormat.experimentRelation, DataFlowFormat.parentRelation)

  val traverseMaxDepth: Int = 50
  val traverseMaxLimit: Long = 250
}

class ViewActor (val dbActor: ActorRef) extends Actor with ActorLogging {

  import ViewActor._

  import context.dispatcher

  def receive: Receive = {
    case ViewProtocol.GetDFF(id: Long) =>
      val requester = context.sender()
      val request = dbActor.ask(DBActor.Load(id))(dbTimeout)

      request.onSuccess {
        case DBActor.Loaded(dff) =>
          requester ! ViewProtocol.DFF(dff)
      }

      request.onFailure {
        case e: Throwable =>
          requester ! ViewProtocol.DFF(DataFlowFormat.errorDff("Error", e))
      }

    case ViewProtocol.Traverse(startId, depth, limit) =>
      val requester = context.sender()
      val request = dbActor.ask(DBActor.Traverse(startId, depth min traverseMaxDepth,
                                                 limit min traverseMaxLimit, traverseRelations))(dbTimeout)

      request.onSuccess {
        case DBActor.Traversed(dff) =>
          requester ! ViewProtocol.Traversed(dff)
      }

      request.onFailure {
        case e: Throwable =>
          requester ! ViewProtocol.Traversed(DataFlowFormat.errorDff("Error", e))
      }
  }
}
