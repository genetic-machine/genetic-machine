package org.geneticmachine.navigation

import org.geneticmachine.navigation.Direction._
import org.geneticmachine.navigation._
import org.geneticmachine.navigation.generators.{ConstantGenerator, RandomWalkGenerator}
import org.geneticmachine.navigation.utils.NavigationInfo
import org.scalatest._
import breeze.linalg.DenseMatrix

class NavigationUtilsSpec extends FlatSpec with Matchers {
  "optimized reversed cost function" must "work right" in {
    val labGen = ConstantGenerator("src/main/resources/labs/simple.lab")
    val (lab, rp, goal) = labGen()

    val costs = reversedCostDict(lab, goal)

    def costMapToString(lab: DenseMatrix[Int]): String = {
      val maxWidth = scala.math.log10(breeze.linalg.max(lab)).ceil.toInt
      val pattern = s"%0${maxWidth}d"
      val format = { x: Int => pattern.format(x) }

      (for {
        i <- 0 until lab.rows
      } yield {
        (for {
          j <- 0 until lab.cols
          c = if (lab(i, j) > -1) format(lab(i, j)) else " " * maxWidth
        } yield c).mkString(" ")
      }).mkString("\n")
    }

    def optimized(rp: RobotPosition): RobotPosition = {
      rp.action(lab)(optimalAction(lab, rp, goal, costs))
    }

    def trajectory(projection: RobotPosition => RobotPosition)
                  (rp: RobotPosition, acc: List[RobotPosition]): List[RobotPosition] = {
      val newPos = projection(rp)

      if (newPos.point != goal && acc.size < 200) {
        trajectory(projection)(newPos, newPos :: acc)
      } else {
        newPos :: acc
      }
    }

    val hist = trajectory(optimized)(rp, List.empty)

    println {
      NavigationInfo.getFig(lab, hist, goal)
    }

    println {
      s"Path length: ${hist.size}"
    }

    println()

    println {
      NavigationInfo.horzConcat("N", "E")(costMapToString(costs(0)), costMapToString(costs(1)))
    }

    println()

    println {
      NavigationInfo.horzConcat("S", "W")(costMapToString(costs(2)), costMapToString(costs(3)))
    }

    val maxTurnDifference = (for {
      i <- 0 until lab.rows
      j <- 0 until lab.cols
    } yield {
      val cs = for {
        d <- 0 until 4
      } yield costs(d)(i, j)

      cs.max - cs.min
    }).max

    println {
      s"Max difference of cost: $maxTurnDifference"
    }
  }

  "SimpleNavigationGenerator" must "return a labyrinth with exit" in {
    val (rows, cols) = (1001, 1001)
    val (lab, start, goal) = RandomWalkGenerator(3, 5)(Point(rows, cols))()
    val cost = reversedCostDict(lab, start.point)

    assert((0 until 4).forall { d => cost(d)(goal.x, goal.y) != Int.MaxValue })
  }
}
