import akka.actor.{Props, ActorSystem}
import robot._
import robot.labyrinth._

object Main extends App {

  val geneticMachine = ActorSystem("genetic-machine")
  val brain = geneticMachine.actorOf(Props(classOf[DijkstraBrain]))
  val robot = geneticMachine.actorOf(Props(new LabyrinthRobot(brain, 10, 10, 5)))
}