package geneticmachine.labyrinth.vision

import breeze.linalg.DenseMatrix
import scala.math._
import geneticmachine.labyrinth.{ Labyrinth, Point, CellStatus, RobotPosition}

object SimpleVision {
  def apply(depth: Int) = new SimpleVision(depth)
}

class SimpleVision(val depth: Int) extends Vision {
  def apply(lab: Labyrinth, rp: RobotPosition): Observation = {
    val from = rp.point
    val size = 2 * depth + 1
    val visionMap = DenseMatrix.fill(size, size)(CellStatus.Unknown)

    for {
      x <- 0 until size
      y <- 0 until size
      labX = x - depth + from.x
      labY = y - depth + from.y
      if abs(x - depth) + abs(y - depth) <= depth
    } {
      visionMap(x, y) = if (Point(labX, labY).inBorders(lab.rows, lab.cols)) lab(labX, labY) else CellStatus.Unknown
    }

    Observation(visionMap, rp)
  }

  override def toString: String = s"SimpleVision($depth)"
}