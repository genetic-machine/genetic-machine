package robot

import akka.actor.{ActorLogging, ActorRef, Actor}
import akka.pattern.ask
import akka.util.Timeout
import scala.util.{Failure, Success}
import common.MessageProtocol
import scala.concurrent.Future
import scala.reflect.ClassTag
import java.util.concurrent.TimeUnit

object Robot {
  import MessageProtocol._

  case object Status extends Request
  case object Statistic extends Request

  case class StatusResponse[+StatusT](status: StatusT) extends Response
  case class StatisticResponse[+StatisticT](statistic: StatisticT) extends Response
  case class Finish(brain: ActorRef) extends Response
  case class Failure(e: Throwable, brain: ActorRef) extends Response
}

/**
 * Provides shell for correct [[robot.Brain]] activity.
 * It's the adapter from 'real' world to abstract brain space.
 *
 * @param brain brain to guide. It's not a child of robot, because brain is much more valuable than
 *              it's shell and it could be used outside, for example, in multi-robot.
 */
abstract class Robot[InputT : ClassTag, StatusT : ClassTag, OutputT : ClassTag] (val brain: ActorRef) extends Actor with ActorLogging {

  import context.dispatcher

  def unexpectedResponse(reason: String)(msg: Any) {
    val e = MessageProtocol.UnexpectedResponse(reason, msg)
    log.error("Unexpected response!", e)
    failure(e)
  }

  def failure(e: Throwable) {
    brain ! Brain.Reset
    context.parent ! Robot.Failure(e, brain)
  }

  /**
   * Robot's initialisation.
   * @return initial status and initial brain's input.
   */
  def selfSetup(): Future[(StatusT, InputT)]

  /**
   * The "physics" of the location.
   * Processes current status and brain's output into new status and sensor data.
   * @param status current status
   * @param brainOutput brain's response
   * @return Some(), that includes new robot's status and new brain's input, if goal is not achieved, otherwise None
   */
  def processOutput(status: StatusT, brainOutput: OutputT): Option[(StatusT, InputT)]

  /**
   * Process brain outputs.
   * @param status
   */
  final def guideBrain(status: StatusT): Receive = {
    case Brain.Output(data: OutputT) =>
      log.info(s"Received: ${data.toString}")
      processOutput(status, data) match {
        case Some((newStatus: StatusT, inputData: InputT)) =>
          brain ! Brain.Input(inputData)
          context.become(training(newStatus), discardOld = true)

        case None =>
          context.parent ! Robot.Finish(brain)
          context.unbecome()
      }
  }

  /**
   * Complex behaviour to process all request during the training.
   * Must include guideBrain behaviour!
   * @param status current status of robot, i.e. status of location, robot's position.
   */
  def training(status: StatusT): Receive

  implicit val timeout = Timeout(1, TimeUnit.SECONDS)

  val brainSensorSetup = for {
    result <- brain ? Brain.Sensor(self)
    if result == MessageProtocol.Done
  } yield result

  brainSensorSetup.onComplete {
    case Success(_) =>
      val brainActuatorSetup = brain ? Brain.Actuator(self)
      val generalSetup = for {
        status <- selfSetup()
        actuatorSetup <- brainActuatorSetup
        if actuatorSetup == MessageProtocol.Ready
      } yield status

      generalSetup.onComplete {
        case Failure(e) =>
          println("Act set up!")
          failure(e)

        case Success((status, input)) =>
          println("Act set up!")
          brain ! Brain.Input(input)
          context.become(training(status))
      }

    case Failure(e) =>
      unexpectedResponse("Brain sensor setup")(e)
  }

  def receive: Receive = {
    case other =>
      unexpectedResponse("Message out of initialization!")(other)
  }
}
