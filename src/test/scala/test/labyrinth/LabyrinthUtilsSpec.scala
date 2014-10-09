package test.labyrinth

import geneticmachine.labyrinth._
import geneticmachine.labyrinth.generators.RandomWalkGenerator
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

  "SimpleLabyrinthGenerator" must "return a labyrinth with exit" in {
    val (rows, cols) = (1001, 1001)
    val (lab, start, goal) = RandomWalkGenerator(3, 5)(Point(rows, cols))()
    val cost = costMapWithoutDirection(lab, start.point)

    assert(cost(goal.x, goal.y) != Int.MaxValue)
  }
}
