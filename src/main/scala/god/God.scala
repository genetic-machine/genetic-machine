package god

import akka.actor.{Props, ActorLogging, Actor}
import environment.Environment

object God {
  abstract class Response
  case object Ready extends Response
  case object Fail extends Response

  abstract class Requests
  case object IsReady extends Requests
}

class God(val manualSource: String = "./src/main/resources/WorldCreation.txt") extends Actor with ActorLogging {
  import StoryTeller._
  import God._

  val voice = context.actorOf(Props(new StoryTeller(manualSource)), "TheStoryTeller")
  val environment = context.actorOf(Props[Environment], "environment")

  voice ! Tell {
    """Hello! I'm the local God!
      |I will guide you through brave new world of evolutionary robotics.
      |Let's start with the little act of creation!""".stripMargin
  }

  (1 to 6).foreach { _ =>
    voice ! NextSentence
  }

  def receive = {
    case _ =>
  }
}