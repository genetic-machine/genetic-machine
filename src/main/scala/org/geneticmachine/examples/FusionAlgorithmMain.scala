package org.geneticmachine.examples

import org.geneticmachine._

import navigation._
import navigation.heuristics._
import navigation.algorithm._
import navigation.vision._
import navigation.generators._
import navigation.feedback._
import navigation.metrics._

import genetic._
import genetic.evolution._
import genetic.selector._
import org.geneticmachine.machine.db.Neo4JDriver
import org.geneticmachine.machine.{SimpleExecutionContext, SimpleMachine}
import org.geneticmachine.navigation.utils.NavigationInfo

object FusionAlgorithmMain {

  def main(args: Array[String]) {

    val senses = List(DijkstraSense, DistanceToGoalSense, GoalDirectionSense)
    val sensesLen = senses.map { _.senseNames.size }.sum

    val mutatorParams = new MutatorParams {
      override val additionalCoefsLen: Int = sensesLen
    }
    val mutator = FusionMutator.adaptive(mutatorParams)
    val selector = QuantileSelector[Gene](0.5) & ThresholdSelector[Gene](0.0)
    val evolution = SafeEvolution.adaptive(populationLimit = 100, crossoverLimit = 25,
                                           mutationLimit = 25, generationLimit = 25)(mutator, selector)

    val algorithmGen = FusionAlgorithm(senses: _*)(evolution)

    val vision = SimpleVision(5)
    val labGenerator = ConstantGenerator("src/main/resources/labs/simple.lab")

    val feedback = LaplacePotentialFeedback(1.0, 5.0) & ImpossibleActionFeedback(-25.0) & HistoryFeedback(-25.0)

    val metrics = List(EuclideanDistanceToTarget, ManhattanDistanceToTarget)

    val navigationEnv = NavigationEnvironmentGen(labGenerator)(vision)(feedback)(List(CommandNumberToOptimal, CommandNumber))(metrics)

    import Experiment._

    val experiment = chain(10) {
      algorithmGen -> navigationEnv
    }

    implicit val ec = new SimpleExecutionContext
    implicit val db = new Neo4JDriver("genetic-db")

    val machine = new SimpleMachine

    val result = machine(experiment)

    machine.log.info {
      NavigationInfo(result)
    }
  }
}
