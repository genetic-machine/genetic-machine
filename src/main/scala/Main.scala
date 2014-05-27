import akka.actor.{Props, ActorSystem}
import god.God

object Main extends App {

  val geneticMachine = ActorSystem("genetic-machine")
  val god = geneticMachine.actorOf(Props(new God()), "God")
}