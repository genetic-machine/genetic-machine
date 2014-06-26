package geneticmachine

import akka.actor.{ActorRef, Props, ActorLogging, Actor}

object Environment {
  import common.MessageProtocol._

  case class RestoreBrain(brainID: Long) extends Request
  case class LaunchRobot(robot: Props, brainID: Long) extends Request

  case class BrainReady(brainID: Long) extends Response
  case class RobotReady(robotID: Long) extends Response
}

abstract class Environment extends Actor with ActorLogging {

  val dbActor: ActorRef

  def receive: Receive = {
    case _ =>
  }
}