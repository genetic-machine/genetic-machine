package geneticmachine.labyrinth.vision

import breeze.linalg._
import geneticmachine.labyrinth._

class MatrixPattern(val matrix: DenseMatrix[Double]) extends Pattern {
  def patternRadius: Point = Point((matrix.rows - 1) / 2, (matrix.cols - 1) / 2)

  final def convolution(observation: Observation): Double = {
    val from = observation.from
    val pR = patternRadius
    val maxVisionPoint = Point(observation.visionMap.rows - 1, observation.visionMap.cols - 1)

    val leftBorder = (from - pR) min Point.zero
    val rightBorder = (from + pR) max maxVisionPoint

    val visionMap = observation.visionMap
    val anormed = (for {
      vRow <- leftBorder.x to rightBorder.x
      vCol <- leftBorder.y to rightBorder.y
      Point(pRow, pCol) = pR - from + Point(vRow, vCol)
    } yield matrix(pRow, pCol) * visionMap(vRow, vCol)).sum

    val area = rightBorder - leftBorder
    anormed / area.x / area.y
  }

  final def apply(observation: Observation): Double = convolution(observation)

  def ##(observation: Observation): Double = convolution(observation)

  def +(other: MatrixPattern): MatrixPattern = MatrixPattern {
    this.matrix + other.matrix
  }
}

object MatrixPattern {
  def apply(matrix: DenseMatrix[Double]): MatrixPattern = {
    assert(matrix.cols % 2 == 1, "Pattern should have central point")
    assert(matrix.rows % 2 == 1, "Pattern should have central point")
    new MatrixPattern(matrix)
  }

  def normalized(matrix: DenseMatrix[Double]): MatrixPattern = {
    val maxValue: Double = max(matrix)
    val minValue: Double = min(matrix)

    // it's correct expression despite IDEA thinks differently
    val m: DenseMatrix[Double] = (matrix - minValue) * (2.0 / (maxValue - minValue)) - 1.0

    MatrixPattern {
      m
    }
  }

  def fill(radius: Int)(value: => Double): MatrixPattern = {
    val size = 2 * radius + 1
    val m = DenseMatrix.fill(size, size)(value)
    new MatrixPattern(m)
  }
}
