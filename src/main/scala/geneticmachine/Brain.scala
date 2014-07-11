package geneticmachine

import akka.actor.{Props, ActorLogging, ActorRef, Actor}
import common.dataflow.{DataFlowFormatBuilder, DataFlowFormat}
import scala.concurrent.Future
import scala.reflect.ClassTag
import scala.util.{Failure, Success}
import DataFlowFormat._

import common.{MessageProtocol => MP}

object Brain {

  import MP._

  case class Input[InputT : ClassTag](data: InputT) extends Request
  case class Output[OutputT : ClassTag](data: OutputT) extends Response
  case class Feedback[FeedbackT : ClassTag](data: FeedbackT) extends Request

  case object Reset extends Request

  //def props[T <: Brain](dff: DataFlowFormat): Props = Props(Class[T], dff)
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
  extends Actor with ActorLogging {

  import context.dispatcher

  /**
   * Returns initial state of actor by provided DFF description.
   * Brain must be able to handle Input requests right after the initialization.
   * Not in future just for convinience of not handling the race during deferred initialization.
   */
  def init(dff: DataFlowFormat): StateT

  /**
   * Processes input data.
   * @param state current state of brain.
   * @param inputData incoming data.
   * @return the pair of new brain's state and response.
   */
  def input(state: StateT, inputData: InputT): Future[(StateT, OutputT)]

  /**
   * Processess feedback of previous output.
   * Feedback *isn't* a new world's state, it contains only information about efficiency of previous output.
   * Depending on this information the brain is learned, so feedback is the tool of supervised learning.
   * @param state current state of brain.
   * @param feedbackData feedback data.
   * @return new brain's state.
   */
  def feedback(state: StateT, feedbackData: FeedbackT): Future[StateT]

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
  def reset(state: StateT): Future[StateT]

  /**
   * Saves current state into [[common.dataflow.DataFlowFormat]].
   * @param state current state.
   * @return serialazed state.
   */
  def serialize(state: StateT): Future[DataFlowFormat]

  def serialize_(state: StateT, requester: ActorRef) {
    val serialization = serialize(state)

    serialization.onSuccess {
      case dff: DataFlowFormat =>
        requester ! MP.Serialized(dff)
    }

    serialization.onFailure {
      case e: Throwable =>
        requester ! MP.Fail(e)
    }
  }

  def process(state: StateT): Receive = {
    case Brain.Input(inData: InputT) =>
      val out = input(state, inData)
      val requester = context.sender()

      out.onSuccess {
        case (newState, output) =>
          requester ! Brain.Output(output)
          context.become(process(newState), discardOld = true)
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
          context.become(process(newState), discardOld = true)
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
          context.become(process(newState), discardOld = true)
          requester ! MP.Ready
      }

      resetting.onFailure {
        case e: Throwable =>
          requester ! MP.Fail(e)
      }

    case MP.Serialize =>
      serialize_(state, context.sender())
  }

  final val receive: Receive = {
    val initialState = init(dff)
    process(initialState)
  }
}