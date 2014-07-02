import akka.actor.{ActorRef, Actor, Props, ActorSystem}
import geneticmachine._
import geneticmachine.labyrinth._
import geneticmachine.labyrinth.generators.RandomWalkGenerator
import geneticmachine.labyrinth.vision.SimpleVision
import geneticmachine.labyrinth.feedback._

object Main extends App {
  override def main(args: Array[String]) {
    val geneticMachine = ActorSystem("genetic-machine")
    val labGen = RandomWalkGenerator(3, 5)(Point(101, 101))
    val vision = new SimpleVision(5)
    val feedback = ZeroFeedback

    val guard = geneticMachine.actorOf(Props(new Actor {
      val brain = context.actorOf(Props(new DijkstraBrain(DijkstraBrain.empty)), "brain")
      val robot = context.actorOf(Props(new LabyrinthRobot(brain, labGen, vision, feedback)), "geneticmachine")

      def guide: Receive = {
        case Robot.Finish(`brain`, stats: LabyrinthStatus) =>
          println(labToString(stats.toCharMap))
          context.system.shutdown()
      }

      override def receive: Receive = guide
    }))
  }
}