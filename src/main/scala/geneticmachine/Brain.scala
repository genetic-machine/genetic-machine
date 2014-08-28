package geneticmachine

import akka.actor._
import akka.pattern.pipe
import scala.concurrent.Future
import scala.reflect.ClassTag
import scala.util.{Failure, Success}

import common.dataflow.{DataFlowFormatBuilder, DataFlowFormat}
import DataFlowFormat._

import common.{MessageProtocol => MP}

object Brain {

  import MP._

  case class Input[InputT : ClassTag](data: InputT) extends Request
  case class Output[OutputT : ClassTag](data: OutputT) extends Response
  case class Feedback[FeedbackT : ClassTag](data: FeedbackT) extends Request

  case object Reset extends Request
}

trait BrainFactory[I, O, F] extends Serializable {
  def props(dff: DataFlowFormat): Props
  def empty: DataFlowFormat

  def errorDff(e: Throwable): DataFlowFormat = {
    DataFlowFormat.errorDff(brainLabel, e)
  }
}

/**
 * Base class for brain functionality.
 * Only methods `init`, `input`, `feedback`, `serialize`, `reset` need to be overridden.
 * Designed to use with [[geneticmachine.Robot]] actor.
 *
 * The all methods above, except `init`, must be implemented through `Future` since
 * the brain could communicate with its childs to obtain the response.
 * In this case, it's convinient to use `ask` pattern and future compositions.
 * Otherwise, [[scala.concurrent.Future.successful]] can be used.
 *
 * The usual workflow
 * (the -[msg]->` means direct passing of message `msg`,
 * `~futureF(data)~> msg->` means response with message `msg` as result of concurrent call (through `Future`) of method `futureF`):
 * {{{
 *   robot -Input(inData)-> brain
 *   brain ~input(inData)~> Output(outData)-> robot
 *   robot -Feedback(feedbackData)-> brain
 *   brain ~feedback(feedbackData)~> Ready-> robot
 *   ...
 *   robot -Reset-> brain
 *   brain ~reset()~> Ready-> robot
 *
 *   nextRobot -Input(inData)-> brain
 *   ...
 *
 *   parent -Serialize-> brain
 *   brain ~serialize()~> Serialized(dff) -> parent
 *   stop or continue
 * }}}
 *
 * @param dff description of the initial brain structure.
 * @tparam InputT type of input messages.
 * @tparam OutputT type of output messages.
 * @tparam FeedbackT type of feedback messages
 * @tparam StateT type of inner state.
 */
abstract class Brain[InputT : ClassTag, OutputT : ClassTag,
                     FeedbackT : ClassTag, StateT : ClassTag](val dff: DataFlowFormat)
  extends Actor with ActorLogging with Stash {

  import context.dispatcher

  private case class Init(state: StateT)

  private case class InputProcessDone(state: StateT, output: OutputT)

  private case class FeedbackDone(state: StateT)
  private case class ResetDone(state: StateT)

  /**
   * Returns initial state of actor by provided DFF description.
   * Brain must be able to handle Input requests right after the initialization.
   * Not in future just for convinience of not handling the race during deferred initialization.
   */
  protected def init: Future[StateT]

  /**
   * Processes input data.
   * @param state current state of brain.
   * @param inputData incoming data.
   * @return the pair of new brain's state and response.
   */
  protected def input(state: StateT, inputData: InputT): Future[(StateT, OutputT)]

  /**
   * Processess feedback of previous output.
   * Feedback *isn't* a new world's state, it contains only information about efficiency of previous output.
   * Depending on this information the brain is learned, so feedback is the tool of supervised learning.
   * @param state current state of brain.
   * @param feedbackData feedback data.
   * @return new brain's state.
   */
  protected def feedback(state: StateT, feedbackData: FeedbackT): Future[StateT]

  /**
   * Inner memory reset procedure.
   *
   * During the work with a robot, the brain could accumulate some inner memory
   * about the current environment, e.g. global map of the current labyrinth.
   * After the action, that leads to the desired state,
   * the robot generates `Reset` message instead of new `Input`, that leads to `reset` call.
   * The `reset` call cleans up all data specific for the current robot.
   *
   * Right after resetting brain must be ready for work with new robot.
   *
   * @param state current state.
   * @return new clean state.
   */
  protected def reset(state: StateT): Future[StateT]

  /**
   * Saves current state into [[common.dataflow.DataFlowFormat]].
   * @param state current state.
   * @return serialazed state.
   */
  protected def serialize(state: StateT): Future[DataFlowFormat]

  private final def stashReceive: Receive = {
    case msg =>
      log.debug(s"Normally stash shouldn't be invoked! [${msg.toString.take(50)}]")
      stash()
  }

  private final def failureReceive(activity: String)(requester: ActorRef, currentState: StateT): Receive = {
    case akka.actor.Status.Failure(e: Throwable) =>
      log.error(s"Main activity ($activity) has been failed! [$e]")
      requester ! MP.Fail(e)
      context.become(process(currentState))
      unstashAll()
  }

  private final def processingOutput(requester: ActorRef): Receive = {
    case InputProcessDone(updatedState: StateT, outData: OutputT) =>
      log.debug("Output has been done.")
      requester ! Brain.Output(outData)
      context.become(process(updatedState))
      unstashAll()
  }

  private final def processingFeedback(requester: ActorRef): Receive = {
    case FeedbackDone(updatedState: StateT) =>
      log.debug("Feedback has been done.")
      requester ! MP.Ready
      context.become(process(updatedState))
      unstashAll()
  }

  private final def resetting(requester: ActorRef): Receive = {
    case ResetDone(updatedState: StateT) =>
      log.debug("Reset has been done.")
      requester ! MP.Ready
      context.become(process(updatedState))
      unstashAll()
  }

  private final def process(state: StateT): Receive = {
    case Brain.Input(inData: InputT) =>
      log.debug(s"Input has been received: ${inData.toString.take(30)}")

      (for {
        (updatedState: StateT, output: OutputT) <- input(state, inData)
      } yield InputProcessDone(updatedState, output)) pipeTo self

      context.become {
        processingOutput(context.sender()) orElse
          failureReceive("processing output")(context.sender(), state) orElse
          stashReceive
      }

    case Brain.Feedback(data: FeedbackT) =>
      log.debug(s"Feedback has been received: ${data.toString.take(30)}")

      (for {
        updatedState <- feedback(state, data)
      } yield FeedbackDone(updatedState)) pipeTo self

      context.become {
        processingFeedback(context.sender()) orElse
          failureReceive("processing feedback")(context.sender(), state) orElse
          stashReceive
      }

    case Brain.Reset =>
      log.debug(s"Reset has been received.")

      (for {
        updatedState <- reset(state)
      } yield ResetDone(updatedState)) pipeTo self

      context.become {
        resetting(context.sender()) orElse
          failureReceive("resetting")(context.sender(), state) orElse
          stashReceive
      }

    case MP.Serialize =>
      log.debug(s"Serialize has been received.")
      (for {
        dff <- serialize(state)
      } yield MP.Serialized(dff)) recover {
        case e: Throwable =>
          MP.Fail(e)
      } pipeTo context.sender()
  }

  private final def initialization() {
    (for {
      state <- init
    } yield Init(state)) recover {
      case e: Throwable =>
        MP.InitializationFailed(e)
    } pipeTo self
  }

  final val receive: Receive = {
    case Init(state: StateT) =>
      log.debug(s"Initialization successful!")
      context.become (process(state))
      unstashAll()

    case MP.InitializationFailed(e: Throwable) =>
      context.become(badlyInitialized(e))
      log.error(s"Initialization failed! [$e]")
      unstashAll()

    case msg =>
      /** Due to race condition during initialization, stash here is normal. */
      log.debug("Stash before initialization.")
      stash()
  }

  private final def badlyInitialized(e: Throwable): Receive = {
    case msg =>
      context.sender() ! MP.Fail {
        MP.InitializationFailure {
          new Exception(s"Brain has failed initialization!", e)
        }
      }
  }

  initialization()
}