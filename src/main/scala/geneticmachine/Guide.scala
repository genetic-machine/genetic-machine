package geneticmachine

import akka.actor.SupervisorStrategy.Stop
import akka.actor._
import akka.pattern.{AskTimeoutException, ask}
import geneticmachine.db.DBActor.{Save, Saved}

import scala.concurrent.Future
import scala.concurrent.duration._
import scala.util.{Failure, Success, Try}
import java.util.concurrent.{TimeUnit, TimeoutException}

import common.MessageProtocol
import common.dataflow.DataFlowFormat

object Guide {
  val brainResetTimeout = 10.seconds
  val serializeTimeout = 1.minute
  val dbSaveTimeout = 1.minute

  import MessageProtocol._

  case object Timeout extends Request

  object GuideResult {
    def apply(e: Throwable): GuideResult = {
      val fail = Failure(e)
      GuideResult(fail, fail, fail)
    }

    def apply(brain: ActorRef, e: Throwable): GuideResult = {
      val fail = Failure(e)
      GuideResult(Success(brain), fail, fail)
    }

    def apply(brain: ActorRef, score: Any, e: Throwable): GuideResult = {
      val fail = Failure(e)
      GuideResult(Success(brain), Success(score), fail)
    }

    def apply(brain: ActorRef, score: Any, dff: DataFlowFormat): GuideResult = {
      GuideResult(Success(brain), Success(score), Success(dff))
    }
  }

  case class GuideResult(brain: Try[ActorRef], score: Try[Any],
                         robotDff: Try[DataFlowFormat]) extends Response

  case class GuideException(msg: String, cause: Throwable) extends Exception(msg, cause)
}

/**
 * One-shot actor, produces robot by `robotFactory`, supervises pair robot-brain.
 * It guarantees to answer with [[geneticmachine.Guide.GuideResult]] and stop itself after.
 *
 * The lifecycle is very simple:
 *   once [[MessageProtocol.Init]] is received, `guide` spawns `robot` with help of `robotFactory` and
 *   monitors all events concerned with `brain` and `robot`, finally produces [[geneticmachine.Guide.GuideResult]]
 *   with results and stops itself.
 *
 * After receiving [[MessageProtocol.Init]] it doesn't process any message expect:
 *   [[akka.actor.Terminated]], from `robot` or [[geneticmachine.Guide.Timeout]].
 *
 * The first priority of `Guide` is to save guided `brain` no matter what happens.
 *
 * `Guide` can't fail.
 */
class Guide(val brain: ActorRef, robotFactory: RobotFactory[_, _, _, _], val timeout: Duration)
  extends Actor with ActorLogging {

  import Guide.GuideResult
  import context.dispatcher

  log.debug(s"+Guide with $brain and $robotFactory")

  val timer = if (timeout.isFinite()) {
    val finiteTimeout = FiniteDuration(timeout.toMillis, TimeUnit.MILLISECONDS)
    context.system.scheduler.scheduleOnce(finiteTimeout) {
      context.self ! Guide.Timeout
    }
  } else {
    new Cancellable {
      override def isCancelled: Boolean = true
      override def cancel(): Boolean = true
    }
  }

  override val supervisorStrategy: SupervisorStrategy = OneForOneStrategy() {
    case _: Exception => Stop
  }

  def receive: Receive = {
    case MessageProtocol.Init =>
      val robot = context.actorOf(robotFactory.props(brain), "robot")
      context.become(waitResult(robot, context.sender()))

      context.watch(robot)
      context.watch(brain)

    case _ => ()
  }

  def firstly(x: => Unit)(other: Receive): Receive = {
    case msg =>
      x
      other(msg)
  }

  def waitResult(robot: ActorRef, requester: ActorRef): Receive = firstly { becomeUnavailable(robot) } {
    case msg if context.sender() == robot =>
      finish(robot, requester, msg)

    case Guide.Timeout =>
      val cause = new TimeoutException(s"Guide timeout.")
      panicShutdown(robot, requester, cause)

    case Terminated(`brain`) =>
      val cause = Guide.GuideException("Brain terminated!", new Exception("Brain terminated!"))
      requester ! GuideResult(cause)

    case Terminated(`robot`) =>
      val cause = Guide.GuideException("Robot terminated!", new Exception("Robot terminated!"))
      panicShutdown(robot, requester, cause)

    case _ => ()
  }
  
  def becomeUnavailable(robot: ActorRef) {
    timer.cancel()
    context.unwatch(robot)
    context.unwatch(brain)
    context.become({
      case _ => ()
    }, discardOld = true)
  }

  def finish(robot: ActorRef, requester: ActorRef, msg: Any) {
    log.debug(s"Normal finish for $robot.")

    msg match {
      case Robot.Finish(`brain`, result) =>
        // serialization contains timeouts
        serialize(robot).onComplete {
          case Success(dff) =>
            log.info(s"$brain finished successfully.")
            requester ! GuideResult(brain, result, dff)

          case Failure(e) =>
            log.error(e, "Serialization failed!")
            requester ! GuideResult(brain, result, e)
        }

      case MessageProtocol.Fail(e: Throwable) =>
        log.error(e, "Robot failure!")
        panicShutdown(robot, requester, e)

      case other =>
        val cause = MessageProtocol.UnexpectedResponse("Unacceptable response", other, "Robot.Finish")
        log.error(s"Robot failure with unacceptable response $other!")
        panicShutdown(robot, requester, cause)
    }
  }

  def serialize(robot: ActorRef): Future[DataFlowFormat] = {
    for {
      MessageProtocol.Serialized(dff) <- robot.ask(MessageProtocol.Serialize)(Guide.serializeTimeout)
    } yield dff
  }

  /**
   * Immediately stops robot, tries to save brain.
   */
  def panicShutdown(robot: ActorRef, requester: ActorRef, cause: Throwable) {
    log.error(s"Emergency stop...", cause)

    val reset = brain.ask(Brain.Reset)(Guide.brainResetTimeout)

    reset.onFailure {
      case e: AskTimeoutException =>
        context.stop(brain)
        log.error(s"$brain reset timeout. Brain was stopped disgracefully.", e)

        val ee = Guide.GuideException(s"Reset timeout!", cause)
        requester ! GuideResult(ee)

      case e: Throwable =>
        context.stop(brain)
        log.error(s"$brain reset failed. Brain was stopped disgracefully.", e)

        val ee = Guide.GuideException(s"Reset failed: $e!", cause)
        requester ! GuideResult(ee)
    }

    reset.onSuccess {
      case Success(MessageProtocol.Ready) =>
        log.warning(s"Brain $brain failed, but was successfully reset.")

        val ee = Guide.GuideException(s"Robot failed!", cause)
        requester ! GuideResult(brain, ee)

      case msg =>
        context.stop(brain)
        log.error(s"$brain reset failed with unacceptable response $msg. Brain was stopped disgracefully.")

        val ee = Guide.GuideException(s"Reset failed with unacceptable response $msg!", cause)
        requester ! GuideResult(ee)
    }
  }
}