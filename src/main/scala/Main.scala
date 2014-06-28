import akka.actor.{ActorRef, Actor, Props, ActorSystem}
import common.MessageProtocol
import geneticmachine._
import geneticmachine.labyrinth._
import geneticmachine.labyrinth.generators.RandomWalkGenerator
import geneticmachine.labyrinth.vision.SimpleVision
import geneticmachine.labyrinth.feedback._

object Main extends App {
  val geneticMachine = ActorSystem("genetic-machine")
  val labGen = RandomWalkGenerator(3, 5)(Point(101, 101))
  val vision = new SimpleVision(5)
  val feedback = ZeroFeedback

  val guard = geneticMachine.actorOf(Props(new Actor {
    val brain = context.actorOf(Props(new DijkstraBrain(DijkstraBrain.serialization())), "brain")

    def guide(robot: ActorRef): Receive = {
      case Robot.Finish(`brain`, stats: LabyrinthStatus) =>
        println(labToString(stats.toCharMap))
        context.system.shutdown()
    }

    override def receive: Receive = {
      case MessageProtocol.Ready if sender() == brain =>
        val robot = context.actorOf(Props(new LabyrinthRobot(brain, labGen, vision, feedback)), "geneticmachine")
        context.become(guide(robot), discardOld = false)
    }
  }))
}