package org.geneticmachine.examples

import org.geneticmachine._

import navigation._
import navigation.algorithm.BrownianWalker
import navigation.generators.ConstantGenerator
import navigation.vision.SimpleVision
import navigation.feedback._
import navigation.metrics._

import Experiment._
import org.geneticmachine.machine.db.Neo4JDriver
import org.geneticmachine.machine.{SimpleExecutionContext, SimpleMachine}

object BrownianBrainMain {

  def main(args: Array[String]) {
    val brainFactory = BrownianWalker

    val vision = SimpleVision(5)
    val labGenerator = ConstantGenerator("src/main/resources/labs/simple.lab")
    val feedback = LaplacePotentialFeedback(1.0) & ImpossibleActionFeedback(-1.0)

    val metrics = List(EuclideanDistanceToTarget, ManhattanDistanceToTarget)

    val navigationEnv = NavigationEnvironmentGen(labGenerator)(vision)(feedback)(List.empty)(metrics)

    implicit val context = new SimpleExecutionContext
    implicit val db = new Neo4JDriver("./genetic-machine-db")
    val machine = new SimpleMachine

    val n = 5

    val profiling = for {
      _ <- 0 until n
    } yield {
      val startT = System.currentTimeMillis()
      val r = machine {
        single(brainFactory -> navigationEnv)
      }
      val endT = System.currentTimeMillis()

      val steps = r.head.finalState.get.finalState.history.size

      (endT - startT, steps)
    }

    println {
      profiling.map { case (t, s) => s"Time: ${"%.2f" format t / 1000.0} sec, steps: $s, per step: ${"%.1f" format (t * 1000.0 / s)} nanosec"}.mkString("\n")
    }
  }
}
