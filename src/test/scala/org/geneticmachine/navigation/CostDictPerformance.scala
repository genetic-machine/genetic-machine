package org.geneticmachine.navigation

import breeze.linalg.DenseMatrix
import org.geneticmachine._

import org.geneticmachine.navigation.generators.ConstantGenerator
import org.scalatest._

class CostDictPerformance extends FlatSpec with Matchers {

  def measure(n: Int, k: Int): Double = {
    val emptyLab = Labyrinth.free(n, n)
    val goal = Point(0, 0)
    val costs = Array.fill(4) {
      DenseMatrix.fill[Int](n, n)(-1)
    }

    val t1 = timedK(k) {
      reversedCostDict(emptyLab, goal, costs)

      (0 until 4).foreach { d =>
        (0 until n).foreach { x =>
          (0 until n).foreach { y =>
            costs(d)(x, y) = -1
          }
        }
      }
    }

    t1
  }

  "optimized reversed cost function" must "work right" in {
    val labGen = ConstantGenerator("src/main/resources/labs/office.lab")
    val (lab, rp, goal) = labGen()

    /** Heat up JIT */
    timedK(100) {
      reversedCostDict(lab, goal)
    }

    val time: Double = timedK(1000) {
      reversedCostDict(lab, goal)
    }

    println {
      s"Labyrinth [${lab.rows} x ${lab.cols}]: ${"%.2f" format time} millisec."
    }

    assert(time < 10.0, "Too slow.")
  }

  "optimized reversed cost function" must "work fast" in {
    val cs = (1 to 20).map { i: Int =>
      val n = 20 * i
      measure(n, 10)
      val t = measure(n, 100)
      val complexity = n * n * 4
      (n, t / complexity)
    }

    println {
      cs.map {
        case (n, t) => s"$n: ${ "%.1f" format (t * 1000000) } nanosec"
      }.mkString("\n")
    }
  }
}
