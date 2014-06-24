package geneticmachine

import akka.actor.{ Props, ActorLogging, Actor}

object Environment {
  import common.MessageProtocol._

  case class RestoreBrain(brainID: Long) extends Request
  case class CopyBrain(BrainID: Long) extends Request
  case class LaunchRobot(robot: Props, brainID: Long) extends Request

  case class BrainReady(brainID: Long) extends Response
  case class RobotReady(robotID: Long) extends Response
}

class Environment extends Actor with ActorLogging {

  def receive: Receive = {
    case _ =>
  }
}