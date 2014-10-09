package geneticmachine.labyrinth.vision

import breeze.linalg._
import geneticmachine.labyrinth._

final class MapPattern(val matrix: DenseMatrix[Double]) extends Pattern {

  val statSum: Double = sum(matrix.map { x: Double => scala.math.abs(x) })

  val patternRadius: Point = Point((matrix.rows - 1) / 2, (matrix.cols - 1) / 2)

  /** Some kind of convolution **/
  def *(observation: Observation): Double = {
    val from = observation.from.point
    val maxVisionPoint = Point(observation.visionMap.rows - 1, observation.visionMap.cols - 1)

    // define the area of convolution
    val leftBorder = (from - patternRadius) max Point.zero
    val rightBorder = (from + patternRadius) min maxVisionPoint
    val patternLeftBorder = patternRadius - from

    val visionMap = observation.visionMap

    val anormedConvolution: Double = (for {
      vRow <- leftBorder.x to rightBorder.x
      vCol <- leftBorder.y to rightBorder.y
      Point(pRow, pCol) = Point(vRow, vCol) + patternLeftBorder
    } yield matrix(pRow, pCol) * visionMap(vRow, vCol)).sum

    anormedConvolution / statSum
  }

  def apply(observation: Observation): Double = this * observation

  def +(other: MapPattern): MapPattern = MapPattern {
    this.matrix + other.matrix
  }

  /** Normed sum */
  def #+(other: MapPattern): MapPattern = MapPattern {
    (this.matrix + other.matrix) / 2.0
  }


  /** Value in centered coordinates. **/
  def apply(p: Point): Double = this(p.x, p.y)

  /** Value in centered coordinates. **/
  def apply(x: Int, y: Int): Double = {
    val absX = patternRadius.x + x
    val absY = patternRadius.y + y

    if (absX >= 0 && absY >= 0 && absX < matrix.rows && absY < matrix.cols) {
      matrix(x, y)
    } else {
      0.0
    }
  }

  def update(x: Int, y: Int, value: Double): Unit = {
    val absX = x + patternRadius.x
    val absY = y + patternRadius.y

    if (absX >= 0 && absY >= 0 && absX < matrix.rows && absY < matrix.cols) {
      matrix(absX, absY) = value
    }
  }
}

object MapPattern {
  def apply(matrix: DenseMatrix[Double]): MapPattern = {
    new MapPattern(matrix)
  }

  def normalized(matrix: DenseMatrix[Double]): MapPattern = {
    val maxValue: Double = max(matrix)
    val minValue: Double = min(matrix)

    MapPattern {
      (matrix - minValue) * (2.0 / (maxValue - minValue)) - 1.0
    }
  }

  def fill(radius: Int, value: Double): MapPattern = {
    val size = 2 * radius + 1
    val m = DenseMatrix.fill(size, size)(value)
    new MapPattern(m)
  }

  implicit def matrixAsPattern(m: DenseMatrix[Double]): MapPattern = MapPattern(m)
}
