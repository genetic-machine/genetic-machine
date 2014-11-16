package geneticmachine

import akka.actor.SupervisorStrategy.Stop
import akka.actor._
import common.MessageProtocol
import common.dataflow.DataFlowFormat
import geneticmachine.Experiment.CycleResult
import geneticmachine.db.DBActor._
import akka.pattern._

import scala.collection.immutable.Queue
import scala.concurrent.Future
import scala.reflect.ClassTag
import scala.util.{Failure, Try, Success}

object ExperimentActor {
  import scala.concurrent.duration._

  val guideTimeout = 1.hour
  val dbTimeout = 10.second
  val serializeTimeout = 1.minute

  case class ExperimentResult[S : ClassTag](results: List[CycleResult[S]]) extends MessageProtocol.Response

  case class FinishCycle[S : ClassTag](cycleResult: CycleResult[S]) extends MessageProtocol.Request

  object FinishCycle {
    def apply[S : ClassTag](rr: Try[RobotResult[S]], id: Try[Long]): FinishCycle[S] = {
      new FinishCycle(CycleResult(rr, id))
    }
  }
}

import ExperimentActor.ExperimentResult

class ExperimentActor[S : ClassTag](val experiment: Experiment[_, _, _, S], val dbActor: ActorRef)
  extends Actor with ActorLogging {

  import context.dispatcher

  override val supervisorStrategy: SupervisorStrategy = OneForOneStrategy() {
    case _: Exception => Stop
  }

  def loadBrain(requester: ActorRef) {
    if (experiment.startWith.isDefined) {
      val id = experiment.startWith.get
      /** dbActor returns dff already injected with its id. **/
      val load = for {
        Loaded(dff) <- dbActor.ask(Load(id))(ExperimentActor.dbTimeout)
      } yield Success(dff)

      load.recover {
        case e: Throwable =>
          Failure(e)
      }.pipeTo(self)(requester)

    } else {
      val emptyDff = experiment.brainFactory.empty
      /** But here injection is needed. **/
      val save = for {
        Saved(id) <- dbActor.ask(Save(emptyDff))(ExperimentActor.dbTimeout)
        injected = emptyDff.idInjection(id)
      } yield Success(injected)

      save.recover {
        case e: Throwable =>
          Failure(e)
      }.pipeTo(self)(requester)
    }
  }

  def execute(requester: ActorRef, brain: ActorRef,
              queue: Queue[RobotFactory[_, _, _, S]], results: List[CycleResult[S]],
              lastBrainId: Long) {

    def launchGuide(robotFactory: RobotFactory[_, _, _, S]): ActorRef = {
      val guide = context.actorOf(Props(new Guide(brain, robotFactory, ExperimentActor.guideTimeout)), "guide")
      log.debug(s"Guide $guide was launched!")
      guide
    }

    queue.dequeueOption match {
      case Some((robotFactory, rest)) =>
        val guide = launchGuide(robotFactory)
        context.watch(guide)
        guide ! MessageProtocol.Init
        context.become(waitResults(requester, brain, robotFactory, rest, results, lastBrainId, guide))

      case None =>
        context.become(receive)
        context.unwatch(brain)
        context.stop(brain)
        log.info(s"Experiment finished. Last brain id: $lastBrainId")
        log.debug(s"Requester: $requester")
        requester ! ExperimentResult(results.reverse)
    }
  }

  def panicShutdown(requester: ActorRef, rest: Queue[RobotFactory[_, _, _, S]],
                    results: List[CycleResult[S]], cause: Throwable) {

    val f = CycleResult(Failure(cause), Failure(cause))
    log.error(cause, "panic shutdown!")
    val finalResults = results.reverse ::: rest.map { _ => f }.toList
    requester ! ExperimentResult(finalResults)
    context.become(receive)
  }

  def serialize(brain: ActorRef, parentId: Long,
                robotFactory: RobotFactory[_, _, _, S], robotDff: Try[DataFlowFormat]): Future[Long] = {
    val brainSerialization = (for {
      MessageProtocol.Serialized(dff) <- brain.ask(MessageProtocol.Serialize)(ExperimentActor.serializeTimeout)
    } yield dff).recover {
      case e: Throwable =>
        experiment.brainFactory.errorDff(e)
    }.recover {
      case e: Throwable =>
        DataFlowFormat.errorDff(DataFlowFormat.brainLabel, e)
    }

    val brainSaving = for {
      dff <- brainSerialization
      injected = dff.parentInjection(parentId)
      Saved(id) <- dbActor.ask(Save(injected))(ExperimentActor.dbTimeout)
    } yield id

    for {
      brainId <- brainSaving
      robotId <- saveRobot(robotFactory, robotDff, brainId)
    } yield ()

    brainSaving
  }

  def saveRobot(factory: RobotFactory[_, _, _, S], dff: Try[DataFlowFormat], brainId: Long): Future[Long] = {
    val recovered = dff.recover {
      case e: Throwable =>
        factory.errorDff(e)
    }.recover {
      case e: Throwable =>
        DataFlowFormat.errorDff(DataFlowFormat.robotLabel, e)
    }.get

    val injected = recovered.uniqueRelationInjection(DataFlowFormat.experimentRelation, brainId)

    for {
      Saved(id) <- dbActor.ask(Save(injected))(ExperimentActor.dbTimeout)
    } yield id
  }

  def waitResults(requester: ActorRef, brain: ActorRef, currentFactory: RobotFactory[_, _, _, S],
                  rest: Queue[RobotFactory[_, _, _, S]], results: List[CycleResult[S]], lastBrainId: Long,
                  guide: ActorRef): Receive = {
    case t@Terminated(`brain`) =>
      context.unwatch(guide)
      context.stop(guide)
      val cause = new MessageProtocol.UnexpectedResponse("Brain terminated!", t, "Some result")
      panicShutdown(requester, rest.enqueue(currentFactory), results, cause)

    case Guide.GuideResult(Failure(e), _, _) =>
      log.error(e, s"Guide lost brain!")
      context.unwatch(guide)
      context.stop(guide)
      panicShutdown(requester, rest.enqueue(currentFactory), results, e)

    /** assume by default that brain is still alive. **/
    case t@Terminated(`guide`) =>
      log.error(s"Guide $guide terminated! This must not happen!")
      val cause = new MessageProtocol.UnexpectedResponse("Guide terminated!!!", t, "Some result")
      val r: Try[RobotResult[S]] = Failure(cause)

      serialize(brain, lastBrainId, currentFactory, Failure(cause)).onComplete {
        case id: Try[Long] =>
          context.self ! ExperimentActor.FinishCycle(r, id)
      }

    case Guide.GuideResult(Success(`brain`), r: Try[RobotResult[S]], robotSerialization) =>
      context.unwatch(guide)
      context.stop(guide)

      (for {
        id <- serialize(brain, lastBrainId, currentFactory, robotSerialization)
      } yield ExperimentActor.FinishCycle(CycleResult(r, Success(id)))).recover {
        case e: Throwable =>
          ExperimentActor.FinishCycle(r, Failure(e))
      } pipeTo context.self

    case Guide.GuideResult(Success(`brain`), strangeResult, robotSerialization) =>
      context.unwatch(guide)
      context.stop(guide)
      val r: Try[RobotResult[S]] = Failure(new MessageProtocol.UnexpectedResponse("Result type mismatch", strangeResult, "Try[S]"))
      serialize(brain, lastBrainId, currentFactory, robotSerialization).onComplete {
        case id: Try[Long] =>
          context.self ! ExperimentActor.FinishCycle(r, id)
      }

    case ExperimentActor.FinishCycle(CycleResult(r: Try[RobotResult[S]], id: Try[Long])) =>
      if (id.isFailure) {
        log.warning(s"Brain serialization failed! $id Continue without saving.")
      }

      execute(requester, brain, rest, CycleResult(r, id) :: results, id.getOrElse(lastBrainId))

    case msg =>
      log.warning(s"Unexpected message during experiment: $msg.")
  }

  def receive: Receive = {
    case MessageProtocol.Init =>
      log.debug("Experiment was initialized.")
      loadBrain(context.sender())

    case Success(dff: DataFlowFormat) =>
      log.debug("Brain was loaded.")
      if (dff.id.isDefined) {
        val brain = context.actorOf(experiment.brainFactory.props(dff), "Brain")
        context.watch(brain)
        execute(context.sender(), brain, experiment.cycles, Nil, dff.id.get)
      } else {
        val cause = new MessageProtocol.InitializationFailure(new Exception(s"${dff.id} Brain DFF has no id! $dff"))
        panicShutdown(context.sender(), experiment.cycles, Nil, cause)
      }

    case msg@Failure(e: Throwable) =>
      val cause = new MessageProtocol.InitializationFailure(e)
      panicShutdown(context.sender(), experiment.cycles, Nil, cause)

    case msg =>
      log.debug(s"Unknown response $msg!")
  }
}
