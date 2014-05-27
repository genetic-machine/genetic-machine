package environment

import akka.actor.Actor
import god.God.Ready
import scala.concurrent.Future

class MapNode(val sizeX: Int, val sizeY: Int, val labGen: (Int, Int) => Labyrinth) extends Actor {

  val labyrinth = simpleLabyrinthGen(sizeX, sizeY)

  context.parent ! Ready

  def receive: Receive = {
    case _ =>
  }
}
