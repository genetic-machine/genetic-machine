package god

import akka.actor.{ActorRef, ActorLogging, Actor}
import scala.concurrent.Future
import akka.pattern.pipe
import scala.io.Source

object StoryTeller {
  case object NextSentence
  case class StoryToTell(text: Array[String])
  case class Tell(text: String)
}

class StoryTeller(val textSource: String) extends Actor with ActorLogging {
  import context.dispatcher
  import StoryTeller._
  import God._

  def tell(str: String) {
    log.info("\n" + { str.split("\n").map { line => s"    $line" } mkString "\n" })
  }

  Future {
    val text: String = Source.fromFile(textSource).mkString
    val sentences: Array[String] = text.split("\\s?\\d+\\s").map { _.trim }.filter { !_.isEmpty }
    StoryToTell(sentences)
  } pipeTo self

  def receive = waitingStory(List[ActorRef]()) orElse speaking

  def speaking: Receive = {
    case Tell(text) =>
      tell(text)
      sender ! Ready
  }

  def waitingStory(nextSentenceQueue: List[ActorRef]): Receive = {
    case StoryToTell(text) =>
      context.become {
        tellingTheStory(text, 0) orElse speaking
      }

      nextSentenceQueue.foreach { actor =>
        self.tell(NextSentence, actor)
      }

    case NextSentence =>
      context.become {
        waitingStory(sender :: nextSentenceQueue) orElse speaking
      }
  }

  def tellingTheStory(text: Array[String], sentenceN: Int): Receive = {
    case NextSentence if sentenceN < text.length =>
      tell(text(sentenceN))
      sender ! Ready
      context.become {
        tellingTheStory(text, sentenceN + 1) orElse speaking
      }

    case NextSentence =>
      sender ! Fail

    case StoryToTell(newText) =>
      context.become {
        tellingTheStory(newText, 0) orElse speaking
      }
  }

}
