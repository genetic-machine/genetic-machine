import akka.actor.Actor.Receive
import akka.actor.{Actor, Props, ActorSystem}
import robot._
import robot.labyrinth._
import robot.labyrinth.generators.RandomWalkGenerator
import robot.labyrinth.vision.SimpleVision

object Main extends App {
  val geneticMachine = ActorSystem("genetic-machine")
  val brain = geneticMachine.actorOf(Props(new DijkstraBrain()), "brain")
  val labGen = RandomWalkGenerator(3, 5)(101, 101)
  val vision = new SimpleVision(5)

  val guard = geneticMachine.actorOf(Props(new Actor {
    val robot = context.actorOf(Props(new LabyrinthRobot(brain, labGen, vision)), "robot")

    override def receive: Receive = {
      case Robot.Finish(`brain`, status: LabyrinthStatus) =>
        println("Finish!")
        println(status.printVisionMap)
        geneticMachine.shutdown()

      case other =>
        println(s"Error: $other")
        geneticMachine.shutdown()
    }
  }))
}