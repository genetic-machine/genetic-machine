package org.geneticmachine.navigation

import breeze.linalg.DenseMatrix
import breeze.storage.Zero
import org.geneticmachine.navigation.CellStatus._

import scala.math._
import scala.reflect.ClassTag

/**
 * It is not a complex value, it represents 2D vector.
 * 10x faster creation than through Vector[Int]
 */
final case class Point(x: Int, y: Int) {

  override def toString: String = s"($x, $y)"

  /** Translation operators **/
  def left: Point =  Point(x - 1, y)

  def right: Point = Point(x + 1, y)

  def backward: Point = Point(x, y - 1)

  def forward: Point = Point(x, y + 1)

  def neighbors: Seq[Point] = Seq(left, right, backward, forward)

  def +(num: Int): Point = Point(this.x + num, this.y + num)

  def +(other: Point) = Point(this.x + other.x, this.y + other.y)

  def -(num: Int): Point = this + (-num)

  def -(other: Point) = Point(this.x - other.x, this.y - other.y)

  /** Rotation operators **/
  def turnLeft: Point = Point(-y ,x)

  def turnRight: Point = Point(y, -x)

  def reverse: Point = Point(-x, -y)

  /** But it is the complex multiplication, i.e. rotation. **/
  def *(other: Point) = Point(this.x * other.x - this.y * other.y,
    this.y * other.x + this.x * other.y)

  def *(scale: Int) = Point(this.x * scale, this.y * scale)

  /** Vector multiplication **/
  def <*>(other: Point): Int = this.x * other.x + this.y * other.y

  def max(other: Point) = Point(this.x max other.x, this.y max other.y)
  def min(other: Point) = Point(this.x min other.x, this.y min other.y)

  /** It isn't the complex division, but rotation in the direction opposite to `other`! **/
  def /(other: Point) = this * other.adjoint

  def adjoint: Point = Point(x, -y)

  /** Norm operators **/
  def lInfNorm: Int = abs(x) max abs(y)

  def l1Norm: Int = abs(x) + abs(y)

  def l2Norm: Double = {
    val xDouble = x.toDouble
    val yDouble = y.toDouble
    sqrt(xDouble * xDouble + yDouble * yDouble)
  }

  def normed: (Double, Double) = {
    val z = l2Norm
    (this.x / z, this.y / z)
  }

  /**
   * Is an equalent to (this / this.l2Norm) <*> (other / other.l2Norm)
   */
  def angle(other: Point): Double = {
    val (x, y) = this.normed
    val (tX, tY) = other.normed

    x * tX + y * tY
  }

  /** Labyrinth operators **/
  def inBorders(sizeX: Int, sizeY: Int): Boolean = (x >= 0) && (x < sizeX) && (y >= 0) && (y < sizeY)

  /** Note: if point is [[Unknown]] cell of `lab` it returns `true`. **/
  def inLabyrinth(lab: Labyrinth): Boolean = inBorders(lab.rows, lab.cols) && (lab(x, y) != Occupied)

  def neighborsInLabyrinth(lab: Labyrinth) = neighbors filter { _.inLabyrinth(lab) }

  /** 'Crop' operator */
  def %(lab: Labyrinth): Point = {
    val x_ = (0 max x) min (lab.rows - 1)
    val y_ = (0 max y) min (lab.cols - 1)
    Point(x_, y_)
  }

  def map(f: (Int, Int) => (Int, Int)): Point = {
    val (tX, tY) = f(x, y)
    Point(tX, tY)
  }
}

object Point {
  val zero = Point(0, 0)
}

object CellStatus {
  final val Free: CellStatus = -1
  final val Occupied: CellStatus = 1
  final val Unknown: CellStatus = 0
  final val startPosition: CellStatus = -2
  final val goal: CellStatus = -3
}

object Labyrinth {
  import CellStatus._

  def unknown(rows: Int, cols: Int): Labyrinth = DenseMatrix.fill[Int](rows, cols)(Unknown)
  def occupied(rows: Int, cols: Int): Labyrinth = DenseMatrix.fill[Int](rows, cols)(Occupied)
  def free(rows: Int, cols: Int): Labyrinth = DenseMatrix.fill[Int](rows, cols)(Free)

  def toArray(lab: Labyrinth): (Array[CellStatus], Int, Int) = matrixToArray(lab)

  def copy[A : ClassTag : Zero](m: DenseMatrix[A], from: Point, to: Point)(zero: A): DenseMatrix[A] = {
    val cm = DenseMatrix.fill[A](to.x - from.x + 1, to.y - from.y + 1)(zero)

    for (i <- (from.x max 0) to (to.x min (m.rows - 1));
         j <- (from.y max 0) to (to.y min (m.cols - 1))) {
      val copyI = i - from.x
      val copyJ = j - from.y
      cm(copyI, copyJ) = m(i, j)
    }

    cm
  }

  def copy[A : ClassTag : Zero](m: DenseMatrix[A], from: Point, r: Int)(zero: A): DenseMatrix[A] = {
    copy(m, from - r, from + r)(zero)
  }

  def centredImpose(f: (CellStatus, Double) => Double)(map: DenseMatrix[CellStatus], from: Point)
                   (pattern: DenseMatrix[Double]): DenseMatrix[Double] = {

    val r = ((pattern.rows - 1) / 2) min ((pattern.cols - 1) / 2)
    val min = (from - r) max Point.zero
    val max = (from + r) min Point(map.rows - 1, map.cols - 1)

    val m = DenseMatrix.zeros[Double](pattern.rows, pattern.cols)

    for (i <- min.x to max.x ; j <- min.y to max.y) {
      val v1 = map(i, j)
      val v2 = pattern(i - min.x, j - min.y)
      m(i - min.x, j - min.y) = f(v1, v2)
    }

    m
  }
}