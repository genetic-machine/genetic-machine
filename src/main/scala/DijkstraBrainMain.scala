import akka.actor.ActorRef
import geneticmachine.ExperimentActor.ExperimentResult
import geneticmachine._
import labyrinth._
import brain._
import genetic._
import vision._
import machine._
import generators._
import metrics._
import feedback._

import scala.util.{Failure, Success, Try}

object DijkstraBrainMain {

  def main(args: Array[String]) {
    val brainFactory = DijkstraBrain

    val vision = SimpleVision(5)
    val labGenerator = ConstantGenerator("src/main/resources/labs/simple.lab")
    val feedback = PotentialLaplaceFeedback(1.0) & ImpossibleActionFeedback(-1.0)

    val metrics = List(EuclideanDistanceToTarget, ManhattanDistanceToTarget)

    val robotFactory = LabyrinthRobotFactory(labGenerator)(vision)(feedback)(List.empty)(metrics)

    val experiment = using(brainFactory).startWithNew.testWith(robotFactory).repeat(5)

    val machine = new GeneticMachine with Neo4jDB

    import machine.dispatcher

    import labyrinth.utils.LabyrinthInfo

    machine(experiment).onComplete {
      case Success(result) =>
        machine.log(LabyrinthInfo(result))
        machine.shutdown()

      case Failure(e: Throwable) =>
        machine.logError(e)
        machine.shutdown()
    }
  }
}
