package org.geneticmachine.examples

import org.geneticmachine._

import navigation._
import algorithm.NaiveNavigation
import navigation.feedback.{ImpossibleActionFeedback, LaplacePotentialFeedback}
import navigation.generators.ConstantGenerator
import navigation.vision._
import navigation.metrics._

import machine.{SimpleMachine, SimpleExecutionContext}
import machine.db.Neo4JDriver
import Experiment._
import org.geneticmachine.navigation.utils.NavigationSVG

object DijkstraAlgorithmMain {
  def main(args: Array[String]) {

    val labGenerator = ConstantGenerator("src/main/resources/labs/office.lab")
    val feedback = LaplacePotentialFeedback(1.0) & ImpossibleActionFeedback(-1.0)

    val metrics = List(EuclideanDistanceToTarget, ManhattanDistanceToTarget)

    val algorithm = NaiveNavigation
    val environment = { vision: Vision =>
      NavigationEnvironmentGen(labGenerator)(vision)(feedback)(List(CommandNumberToOptimal, CommandNumber))(metrics)
    }

    implicit val db = new Neo4JDriver("./genetic-machine-db")
    implicit val context = new SimpleExecutionContext

    val machine = new SimpleMachine

    val r = machine {
      sequence {
        (1 until 5).map { _ * 5 }.map { r: Int =>
          step(algorithm -> environment(SimpleVision(r)))
        }
      }
    }

    NavigationSVG(r).save("./result.svg")
  }
}
