import akka.actor.{Props, ActorSystem}
import robot._
import robot.labyrinth._

object Main extends App {
  val geneticMachine = ActorSystem("genetic-machine")
  geneticMachine.shutdown()
}