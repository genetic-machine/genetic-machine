package org.geneticmachine.navigation

import org.geneticmachine.navigation.Direction._
import org.geneticmachine.navigation._
import org.geneticmachine.navigation.generators.{ConstantGenerator, RandomWalkGenerator}
import org.geneticmachine.navigation.utils.NavigationInfo
import org.scalatest._
import breeze.linalg.DenseMatrix

class LabyrinthUtilsSpec extends FlatSpec with Matchers {

  def cellFromChar(c: Char): CellStatus = c match {
    case '#' => CellStatus.Occupied
    case '`' => CellStatus.Unknown
    case _ => CellStatus.Free
  }

  def costFromChars(labC: Seq[String]): Labyrinth = {
    val rows = labC.length
    val cols = labC(0).length

    val lab = DenseMatrix.fill[Int](rows, cols)(Int.MaxValue)

    for {
      x <- 0 until rows
      y <- 0 until cols
      if labC(x)(y).isDigit
    } {
      lab(x, y) = labC(x)(y).toString.toInt
    }
    lab
  }

  def labFromChars(labC: Seq[String]): Labyrinth = {
    val rows = labC.length
    val cols = labC(0).length

    val lab = Labyrinth.occupied(rows, cols)
    for {
      x <- 0 until rows
      y <- 0 until cols
    } {
      lab(x, y) = cellFromChar(labC(x)(y))
    }

    lab
  }

  "costMap" must "work right" in {
    val lab = Seq(
      "0#45",
      "123#"
    )

    assert(costMapWithoutDirection(labFromChars(lab), Point(0, 0)) == costFromChars(lab))
  }

  "optimized reversed cost function" must "work right" in {
    val labGen = ConstantGenerator("src/main/resources/labs/simple.lab")
    val (lab, rp, goal) = labGen()

    val rDirs = Map(0 -> North, 1 -> East, 2 -> South, 3 -> West)
    val dirs = Map(North -> 0, East -> 1, South -> 2, West -> 3)

    def dictToMaps(dict: Map[RobotPosition, Int]): Array[DenseMatrix[Int]] = {
      val cs = Array.fill(4) {
        DenseMatrix.fill[Int](lab.rows, lab.cols)(-1)
      }

      for ((RobotPosition(Point(x, y), dir), cost) <- dict) {
        val d = dirs(dir)
        cs(d)(x, y) = cost
      }

      cs
    }

    val costs = optimizedReverseCostDict(lab, goal)
    val nCosts = dictToMaps(reverseCostDict(lab, goal))

    val distCosts = reverseCostDict(lab, goal)

    val eq = for {
      dir <- 0 until 4
      x <- 0 until lab.rows
      y <- 0 until lab.cols
      rp = RobotPosition(Point(x, y), rDirs(dir))
      if distCosts.contains(rp)
      if costs(dir)(x, y) != distCosts(rp)
    } yield (rp, costs(dir)(x, y), distCosts(rp))

    println {
      eq.mkString("\n")
    }

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

    def projection(rp: RobotPosition): RobotPosition = {
      rp.action(lab)(optimalAction(lab, rp, goal, costs))
    }

    def trajectory(rp: RobotPosition, acc: List[RobotPosition]): List[RobotPosition] = {
      val newPos = projection(rp)

      if (newPos.point != goal && acc.size < 200) {
        trajectory(newPos, newPos :: acc)
      } else {
        newPos :: acc
      }
    }

    def r(rp: RobotPosition): String = {

      val xs = rp.actions(lab).map {
        case (action, RobotPosition(Point(x, y), dir)) =>
          (action, costs(dirs(dir))(x, y))
      }.map {
        case (act, c) =>
          s"${NavigationCommand.char(act)}: $c"
      }.mkString(" ")

      s"${rp.point.x} ${rp.point.y} ${Direction.char(rp.direction)}: $xs"
    }

    val hist = trajectory(rp, List.empty)

    println {
      hist.reverse.map(r).mkString("\n")
    }

    println {
      NavigationInfo.getFig(lab, hist, goal)
    }

    println {
      val cLab = labToCharMatrix(lab)
      cLab(rp.point.x, rp.point.y) = Direction.char(rp.direction)
      cLab(goal.x, goal.y) = 'X'
      charMatrixToString(cLab)
    }

    println()

    println {
      NavigationInfo.horzConcat("N", "E")(costMapToString(costs(0)), costMapToString(costs(1)))
    }

    println()

    println {
      NavigationInfo.horzConcat("S", "W")(costMapToString(costs(2)), costMapToString(costs(3)))
    }

    for (i <- 0 until 4) {
      println {
        NavigationInfo.horzConcat("", "")(costMapToString(costs(i)), costMapToString(nCosts(i)))
      }

      println()
    }
  }

  "SimpleLabyrinthGenerator" must "return a labyrinth with exit" in {
    val (rows, cols) = (1001, 1001)
    val (lab, start, goal) = RandomWalkGenerator(3, 5)(Point(rows, cols))()
    val cost = costMapWithoutDirection(lab, start.point)

    assert(cost(goal.x, goal.y) != Int.MaxValue)
  }
}
