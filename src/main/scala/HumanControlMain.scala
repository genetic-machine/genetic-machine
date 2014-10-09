import geneticmachine._
import geneticmachine.labyrinth.LabyrinthRobotFactory
import geneticmachine.labyrinth.brain.HumanControl
import geneticmachine.labyrinth.feedback.{ImpossibleActionFeedback, PotentialLaplaceFeedback}
import geneticmachine.labyrinth.generators.ConstantGenerator
import geneticmachine.labyrinth.metrics.{ManhattanDistanceToTarget, EuclideanDistanceToTarget}
import geneticmachine.labyrinth.vision.SimpleVision
import geneticmachine.machine.{RemoteView, RemoteControl, Neo4jDB, GeneticMachine}

object HumanControlMain {

  def main(args: Array[String]) {
    val brainFactory = HumanControl

    val vision = SimpleVision(5)
    val labGenerator = ConstantGenerator("src/main/resources/labs/simple.lab")
    val feedback = PotentialLaplaceFeedback(1.0) & ImpossibleActionFeedback(-1.0)

    val metrics = List(EuclideanDistanceToTarget, ManhattanDistanceToTarget)

    val robotFactory = LabyrinthRobotFactory(labGenerator)(vision)(feedback)(List.empty)(metrics)

    val experiment = using(brainFactory).startWithNew.testWith(robotFactory).repeat(5)

    val machine = new GeneticMachine with Neo4jDB with RemoteControl with RemoteView

    machine(experiment)
  }
}
