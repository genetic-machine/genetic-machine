package org.geneticmachine.navigation.generators

import org.geneticmachine.navigation._
import scala.io.Source

case class ConstantGenerator(filename: String) extends LabyrinthGenerator {

  val (lab, rp, goal) = {
    val lab: Array[Array[Char]] = Source.fromFile(filename).getLines().filter { line: String =>
      line.size > 1
    }.toArray.map { line: String =>
      line.filter { c: Char => !c.isSpaceChar }.toArray
    }

    val rows = lab.size
    val cols = lab.map { _.size }.max

    val matrix: Labyrinth = Labyrinth.unknown(rows, cols)

    for (i <- 0 until rows; j <- 0 until cols) {
      matrix(i, j) = lab(i)(j) match {
        case '#' => CellStatus.Occupied
        case _ => CellStatus.Free
      }
    }

    val goalX = lab.indexWhere { line: Array[Char] =>
      line.contains('*')
    }

    val goalY = lab(goalX).indexOf('*')
    val dirs = Direction.directions.map { Direction.id }.toSet

    val (sx, sy, dir) = (for {
      i <- 0 until rows
      j <- 0 until cols
      if dirs.contains(lab(i)(j))
    } yield (i, j, Direction.fromId(lab(i)(j)))).head

    (matrix, RobotPosition(Point(sx, sy), dir), Point(goalX, goalY))
  }

  def apply(): (Labyrinth, RobotPosition, Point) = {
    (lab, rp, goal)
  }

}
