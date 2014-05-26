package geneticmachine.god

import akka.actor.{Props, ActorLogging, Actor}

object God {
  case object Done
  case object Fail
}

class God(val manualSource: String = "./src/main/resources/WorldCreation.txt") extends Actor with ActorLogging {
  import StoryTeller._
  import God._

  def oneShot(act: => Unit, recover: => Unit = {}): Receive = {
    case Done =>
      act
      context.unbecome()
    case Fail =>
      context.unbecome()
  }

  context.become(
    oneShot {
      println("Create Light: DONE.")
    }, discardOld = false
  )

  context.become(
    oneShot {
      println("Create env: DONE.")
    }, discardOld = false
  )

  context.become(
    oneShot {
      println("Introduce myself: DONE.")
    }, discardOld = false
  )

  val voice = context.system.actorOf(Props(new StoryTeller(manualSource)), "TheStoryTeller")

  voice ! Tell {
    """Hello! I'm the local God!
      |I will guide you through brave new world of evolutionary robotics.
      |Let's start with the little act of creation!
    """.stripMargin
  }

  voice ! NextSentence
  voice ! NextSentence

  def receive = {
    case pray => 
  }
}