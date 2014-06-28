package geneticmachine

import java.util.concurrent.TimeUnit

import akka.actor.{Props, ActorRef, ActorLogging, Actor}
import akka.pattern.ask
import akka.util.Timeout
import common.MessageProtocol

import geneticmachine.dataflow.DataFlowFormat
import geneticmachine.db.DBActor._

import scala.concurrent.Future
import scala.util.{Failure, Success}

object Experiment {
  type BrainGen = (Option[DataFlowFormat]) => Props
  type RobotGen = (ActorRef) => Props

  import MessageProtocol._

  case class Finish(brainDFFId: Long, robotDFFId: Long) extends Response
}

import Experiment._

class Experiment(brainDFFId: Option[Long], brainGen: BrainGen, robotGen: RobotGen)(implicit dbActor: ActorRef) extends Actor with ActorLogging {
  import context.dispatcher

  implicit val timeout: akka.util.Timeout = new Timeout(3, TimeUnit.SECONDS)

  brainDFFId match {
    case Some(id) =>
      (dbActor ? Load(id)).onComplete {
        case Success(Loaded(dff)) =>
          val brain = context.actorOf(brainGen(Some(dff)))
          val robot = context.actorOf(robotGen(brain))
          context.become(initialized(brain, robot))

        case Failure(e: Throwable) =>
          context.parent ! MessageProtocol.Fail(e)

        case msg =>
          context.parent ! MessageProtocol.Fail(MessageProtocol.UnexpectedResponse("DFF load", msg))
      }

    case None =>
      val brain = context.actorOf(brainGen(None))
      val robot = context.actorOf(robotGen(brain))
      context.become(initialized(brain, robot))
  }


  def dffExperimentInjection(dff: DataFlowFormat, brainId: Long): DataFlowFormat = {
    dff.copy(relations = dff.relations + (DataFlowFormat.experimentRelation -> brainId))
  }

  def serialize(brain: ActorRef, robot: ActorRef) : Future[(Long, Long)] = {
    for {
      MessageProtocol.Serialized(brainDff) <- brain ? MessageProtocol.Serialize
      MessageProtocol.Serialized(robotDff) <- robot ? MessageProtocol.Serialize
      Saved(brainId) <- dbActor ? Save(brainDff)
      Saved(robotId) <- dbActor ? Save(dffExperimentInjection(robotDff, brainId))
    } yield (brainId, robotId)
  }

  def initialized(brain: ActorRef, robot: ActorRef): Receive = {
    case Robot.Finish(`brain`, _) =>
      val serialization = serialize(brain, robot)

      serialization.onSuccess {
        case (bId: Long, rId: Long) =>
          context.parent ! Finish(bId, rId)
      }

      serialization.onFailure {
        case e: Throwable =>
          context.parent ! MessageProtocol.Fail(e)
      }
  }

  def receive: Receive = {
    case _ =>
  }
}
