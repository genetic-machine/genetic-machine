package geneticmachine

import akka.actor.{ActorLogging, ActorRef, Actor}
import geneticmachine.ubf.UnifiedBrainFormat
import scala.concurrent.Future
import scala.reflect.ClassTag
import scala.util.{Failure, Success}

import common.{ MessageProtocol => MP }

object Brain {

  import MP._

  case class Input[InputT : ClassTag](data: InputT) extends Request
  case class Output[OutputT : ClassTag](data: OutputT) extends Response
  case class Feedback[FeedbackT : ClassTag](data: FeedbackT) extends Request

  case object Init extends Request
  case object Reset extends Request

  case object Serialize extends Request
  case class Serialized(ubf: UnifiedBrainFormat) extends Response
}

abstract class Brain[InputT : ClassTag, OutputT : ClassTag,
                     FeedbackT : ClassTag, StateT: ClassTag](ubf: UnifiedBrainFormat) extends Actor with ActorLogging {

  import context.dispatcher

  def init(ubf: UnifiedBrainFormat): Future[StateT]

  def process(state: StateT, input: InputT): Future[(StateT, OutputT)]
  def feedback(state: StateT, feedback: FeedbackT): Future[StateT]

  def reset(state: StateT): Future[StateT]

  def serialize(state: StateT): Future[UnifiedBrainFormat]

  def serialize_(state: StateT, requester: ActorRef) {
    val serialization = serialize(state)

    serialization.onSuccess {
      case ubf: UnifiedBrainFormat =>
        requester ! Brain.Serialized(ubf)
    }

    serialization.onFailure {
      case e: Throwable =>
        requester ! MP.Fail(e)
    }
  }

  def initialized(state: StateT): Receive = {
    case Brain.Input(input: InputT) =>
      val out = process(state, input)
      val requester = context.sender()

      out.onSuccess {
        case (newState, output) =>
          requester ! Brain.Output(output)
          context.become(initialized(newState), discardOld = true)
      }

      out.onFailure {
        case e: Throwable =>
          requester ! MP.Fail(e)
      }

    case Brain.Feedback(data: FeedbackT) =>
      val out = feedback(state, data)
      val requester = context.sender()

      out.onSuccess {
        case newState: StateT =>
          context.become(initialized(newState), discardOld = true)
          requester ! MP.Ready
      }

      out.onFailure {
        case e: Throwable =>
          requester ! MP.Fail(e)
      }

    case Brain.Reset =>
      val resetting = reset(state)
      val requester = context.sender()

      resetting.onSuccess {
        case newState: StateT =>
          context.become(initialized(newState), discardOld = true)
          requester ! MP.Ready
          context.parent ! MP.Ready
      }

      resetting.onFailure {
        case e: Throwable =>
          requester ! MP.Fail(e)
      }

    case Brain.Serialize =>
      serialize_(state, context.sender())
  }

  init(ubf).onComplete {
    case Success(state: StateT) =>
      context.become(initialized(state), discardOld = true)
      context.parent ! MP.Ready

    case Failure(e: Throwable) =>
      context.parent ! MP.Fail(MP.InitializationFailure(e))
      context.stop(self)
  }

  final def receive: Receive = {
    case _ =>
      MP.Busy
  }
}