//import akka.actor.ActorRef
//import org.geneticmachine._
//import org.geneticmachine.genetic.evolutions.SafeEvolution
//import org.geneticmachine.genetic.selectors.{ThresholdSelector, QuantileSelector}
//import org.geneticmachine.navigation.heuristics.{GoalDirectionSense, DistanceToGoalSense, DijkstraSense}
//import org.geneticmachine.navigation.utils.{NavigationSVG, NavigationInfo}
//import labyrinth._
//import brain._
//import genetic._
//import vision._
//import machine._
//import generators._
//import metrics._
//import feedback._
//
//import scala.collection.parallel.immutable.ParVector
//
//object FusionBrainMain {
//
//  def main(args: Array[String]) {
//
//    val senses = List(DijkstraSense, DistanceToGoalSense, GoalDirectionSense)
//    val additionalCoefLen = senses.map { _.senseNames.size }.sum
//
//    val mutator = LabyrinthMutator.adaptive(3, additionalCoefLen, 0.1, 2.0, 0.2)
//    val selector = QuantileSelector[Gene](0.5) & ThresholdSelector[Gene](0.0)
//    val evolution = SafeEvolution.adaptive(populationLimit = 1000, crossoverLimit = 250,
//                                           mutationLimit = 250, generationLimit = 250)(mutator, selector)
//
//    val brainFactory = FusionBrainFactory(senses: _*)(evolution)
//
//    val vision = SimpleVision(5)
//    val labGenerator = ConstantGenerator("src/main/resources/labs/simple.lab")
//
//    val feedback = AdvancedPotentialLaplaceFeedback(1.0, 5.0) & ImpossibleActionFeedback(-25.0) & HistoryFeedback(-25.0)
//
//    val metrics = List(EuclideanDistanceToTarget, ManhattanDistanceToTarget)
//
//    val robotFactory = LabyrinthRobotFactory(labGenerator)(vision)(feedback)(List(CommandNumberToOptimal, CommandNumber))(metrics)
//
//    val experiment = using(brainFactory).startWithNew.testWith(robotFactory).repeat(100)
//
//    val machine = new GeneticMachine with Neo4jDB
//
//    import scala.concurrent.ExecutionContext.Implicits.global
//
//    for (results <- machine(experiment)) {
//      machine.log(NavigationInfo(results))
//      NavigationSVG(results).save("result.svg")
//      NavigationSVG(results).saveAsHTML("result.html")
//
//      println {
//        results.results.mkString("\n")
//      }
//
//      machine.shutdown()
//    }
//  }
//}
