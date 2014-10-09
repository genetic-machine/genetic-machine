import akka.actor.ActorRef
import geneticmachine._
import labyrinth._
import brain._
import genetic._
import vision._
import machine._
import generators._
import metrics._
import feedback._

object FusionBrainMain {

  def main(args: Array[String]) {
    val mutator = LabyrinthPatternMutator(3)
    val selector = ThresholdSelect[Gene](0.0)
    val evolution = SafeEvolution[Gene](mutator, selector,
      populationLimit = 1000, crossoverLimit = 750, mutationLimit = 250, generationLimit = 250)

    val brainFactory = FusionBrainFactory(evolution)

    val vision = SimpleVision(5)
    val labGenerator = ConstantGenerator("src/main/resources/labs/simple.lab")//RandomWalkGenerator(5, 5)(Point(31, 31))
    val feedback = AdvancedPotentialLaplaceFeedback(1.0, 25.0) & ImpossibleActionFeedback(-25.0)

    val metrics = List(EuclideanDistanceToTarget, ManhattanDistanceToTarget)

    val robotFactory = LabyrinthRobotFactory(labGenerator)(vision)(feedback)(List.empty)(metrics)

    val experiment = using(brainFactory).startWithNew.testWith(robotFactory).repeat(5)

    val machine = new GeneticMachine with Neo4jDB with RemoteControl with RemoteView

    machine(experiment)
  }
}
