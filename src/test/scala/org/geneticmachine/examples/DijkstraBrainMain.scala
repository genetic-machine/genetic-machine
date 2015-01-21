import akka.actor.ActorRef
import org.geneticmachine.machine._
import ExperimentActor.ExperimentResult
import org.geneticmachine._
import org.geneticmachine.navigation.utils.LabyrinthSVG
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

    val vision = SimpleVision(100)
    val labGenerator = ConstantGenerator("src/main/resources/labs/office.lab")
    val feedback = PotentialLaplaceFeedback(1.0) & ImpossibleActionFeedback(-1.0)

    val metrics = List(EuclideanDistanceToTarget, ManhattanDistanceToTarget)

    val robotFactory = LabyrinthRobotFactory(labGenerator)(vision)(feedback)(List(CommandNumberToOptimal, CommandNumber))(metrics)

    val experiment = using(brainFactory).startWithNew.testWith(robotFactory)

    val machine = new GeneticMachine with Neo4jDB

    import machine.dispatcher

    import labyrinth.utils.LabyrinthInfo

    machine(experiment).onComplete {
      case Success(result) =>
        machine.log(LabyrinthInfo(result))
        LabyrinthSVG(result).save("dijkstra")
        machine.shutdown()

      case Failure(e: Throwable) =>
        machine.logError(e)
        machine.shutdown()
    }
  }
}
