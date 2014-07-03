package geneticmachine.labyrinth.vision

import breeze.linalg.DenseMatrix
import scala.math._
import geneticmachine.labyrinth.{ Labyrinth, Point, CellStatus}

object SimpleVision {
  def apply(deep: Int) = new SimpleVision(deep)
}

class SimpleVision(val deep: Int) extends Vision {
  def apply(lab: Labyrinth, from: Point): Observation = {
    val size = 2 * deep + 1
    val visionMap = DenseMatrix.fill(size, size)(CellStatus.Unknown)

    for {
      x <- 0 until size
      y <- 0 until size
      labX = x - deep + from.x
      labY = y - deep + from.y
      if abs(x - deep) + abs(y - deep) <= deep
    } {
      visionMap(x, y) = if (Point(labX, labY).inBorders(lab.rows, lab.cols)) lab(labX, labY) else CellStatus.Unknown
    }

    Observation(visionMap, from)
  }

  override def toString(): String = s"SimpleVision($deep)"
}