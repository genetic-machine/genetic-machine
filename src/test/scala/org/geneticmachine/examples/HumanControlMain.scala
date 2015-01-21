import org.geneticmachine._
import org.geneticmachine.navigation.LabyrinthRobotFactory
import org.geneticmachine.navigation.algorithm.HumanControl
import org.geneticmachine.navigation.feedback.{ImpossibleActionFeedback, LaplacePotentialFeedback$}
import org.geneticmachine.navigation.generators.ConstantGenerator
import org.geneticmachine.navigation.metrics.{ManhattanDistanceToTarget, EuclideanDistanceToTarget}
import org.geneticmachine.navigation.vision.SimpleVision
import org.geneticmachine.machine.{RemoteView, RemoteControl, Neo4jDB, GeneticMachine}

object HumanControlMain {

  def main(args: Array[String]) {
    val brainFactory = HumanControl

    val vision = SimpleVision(5)
    val labGenerator = ConstantGenerator("src/main/resources/labs/simple.lab")
    val feedback = LaplacePotentialFeedback(1.0) & ImpossibleActionFeedback(-1.0)

    val metrics = List(EuclideanDistanceToTarget, ManhattanDistanceToTarget)

    val robotFactory = LabyrinthRobotFactory(labGenerator)(vision)(feedback)(List.empty)(metrics)

    val experiment = using(brainFactory).startWithNew.testWith(robotFactory).repeat(5)

    val machine = new GeneticMachine with Neo4jDB with RemoteControl with RemoteView

    machine(experiment)
  }
}
