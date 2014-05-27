package environment

import akka.actor.{Props, ActorRef, Actor}
import scala.collection.mutable

object Environment {
  abstract class Request
  abstract class Response

  case object Ready extends Response
  case object Finish extends Response
}

class Environment(val labyrinthPoolSize: Int = 5, val sizeX: Int, val sizeY: Int) extends Actor {
  import god.God
  import Environment._

  context.parent ! God.Ready

  val labyrinths: mutable.Set[ActorRef] = mutable.Set.empty
  val workLabyrinths: mutable.Set[ActorRef] = mutable.Set.empty

  (0 until labyrinthPoolSize).foreach { _ =>
    val labActor = context.actorOf(Props(new MapNode(sizeX, sizeY, simpleLabyrinthGen)))
  }

  def receive: Receive = {
    case Ready =>
      labyrinths += sender()

    case Finish =>
      workLabyrinths -= sender()
  }
}
