package robot

import breeze.linalg.DenseMatrix
import scala.math._
import scala.Ordering
import scala.util.Random

package object labyrinth {

  type CellStatus = Int

  object CellStatus {
    final val Free: CellStatus = -1
    final val Occupied: CellStatus = 1
    final val Unknown: CellStatus = 0
  }

  import CellStatus._

  type Labyrinth = DenseMatrix[CellStatus]

  object Labyrinth {
    def apply(rows: Int, cols: Int) = unknown(rows, cols)
    def unknown(rows: Int, cols: Int): Labyrinth = DenseMatrix.fill[Int](rows, cols)(Unknown)
    def occupied(rows: Int, cols: Int): Labyrinth = DenseMatrix.fill[Int](rows, cols)(Occupied)
    def free(rows: Int, cols: Int): Labyrinth = DenseMatrix.fill[Int](rows, cols)(Free)
  }

  type CostMap = DenseMatrix[Int]

  /**
   * It is not a complex value, it is 2D vector.
   * 10x faster creation than through Vector[Int]
   */
  final case class Point(x: Int, y: Int) {

    /** Translation operators **/
    def left: Point =  Point(x - 1, y)

    def right: Point = Point(x + 1, y)

    def backward: Point = Point(x, y - 1)

    def forward: Point = Point(x, y + 1)

    def neighbors: Seq[Point] = Seq(left, right, backward, forward)

    def +(other: Point) = Point(this.x + other.x, this.y + other.y)

    def -(other: Point) = Point(this.x - other.x, this.y - other.y)

    /** Rotation operators **/
    def turnLeft: Point = Point(-y ,x)

    def turnRight: Point = Point(y, -x)

    def *(other: Point) = Point(this.x * other.x - this.y * other.y,
      this.y * other.x + this.x * other.y)

    def *(scale: Int) = Point(this.x * scale, this.x * scale)

    def max(other: Point) = Point(this.x max other.x, this.y max other.y)
    def min(other: Point) = Point(this.x min other.x, this.y min other.y)

    /** It isn't the complex division! **/
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

    /** Labyrinth operators **/
    def inBorders(sizeX: Int, sizeY: Int): Boolean = (x >= 0) && (x < sizeX) && (y >= 0) && (y < sizeY)

    def inLabyrinth(lab: Labyrinth): Boolean = inBorders(lab.rows, lab.cols) && (lab(x, y) != Occupied)

    def neighborsInLabyrinth(lab: Labyrinth) = neighbors filter { _.inLabyrinth(lab) }

    def %(lab: Labyrinth): Point = {
      val x_ = (0 max x) min (lab.rows - 1)
      val y_ = (0 max y) min (lab.cols - 1)
      Point(x_, y_)
    }
  }

  object Point {
    val zero = Point(0, 0)
  }

  type Path = Seq[Point]

  object Command extends Enumeration {
    type Command = Value
    val TurnLeft, TurnRight, Forward = Value
  }

  type CommandSignal = Map[Command.Command, Double]

  object Direction {

    final val North = Point(1, 0)
    final val South = Point(-1, 0)
    final val West = Point(0, 1)
    final val East = Point(0, -1)

    final val directions = Seq(North, South, West, East)

    type Direction = Point
  }

  import Direction._

  def costMap(lab: Labyrinth, from: Point, deep: Int = Int.MaxValue): DenseMatrix[Int] = {
    val costMap = DenseMatrix.fill(lab.rows, lab.cols)(Int.MaxValue)
    costMap(from.x, from.y) = 0

    def deepFirstSearch(openSet: Set[Point], closedSet: Set[Point], deep: Int) {
      val newOpenSet = for {
        p <- openSet
        n <- p.neighborsInLabyrinth(lab)
        if !closedSet.contains(n)
        if costMap(n.x, n.y) > costMap(p.x, p.y) + 1
      } yield {
        costMap(n.x, n.y) = costMap(p.x, p.y) + 1
        n
      }

      if (newOpenSet.nonEmpty && deep > 0) {
        deepFirstSearch(newOpenSet, closedSet.union(openSet), deep - 1)
      }
    }

    deepFirstSearch(Set[Point](from), Set.empty[Point], deep)
    costMap
  }



  def printLab(lab: Labyrinth): DenseMatrix[Char] = {
    lab map {
      case Free => ' '
      case Occupied => '#'
      case Unknown => '`'
    }
  }

  def directionToCommand(robotDirection: Direction)(direction: Direction): Seq[Command.Command] = {
    val robotDirectionLeft = robotDirection.turnLeft
    val robotDirectionRight = robotDirection.turnRight
    direction match {
      case `robotDirection` => Seq(Command.Forward)
      case `robotDirectionLeft` => Seq(Command.TurnLeft)
      case `robotDirectionRight` => Seq(Command.TurnRight)
      case _ => Seq(Command.TurnLeft, Command.TurnRight)
    }
  }

  def minPathSensor(lab: Labyrinth, from: Point,
                    robotDirection: Direction,
                    cost: CostMap): CommandSignal = {
    val fromCost = cost(from.x, from.y)

    val localCost = for {
      dir <- directions
      p = from + dir
      if p.inLabyrinth(lab)
      c = cost(p.x, p.y) + (dir - robotDirection).lInfNorm
    } yield (dir, c)

    val (_, minCost) = localCost min (Ordering by { x: (Direction, Int) => x._2 })
    val minDirs = localCost filter { _._2 == minCost } map { _._1 }

    (for {
      dir <- minDirs
      command <- directionToCommand(robotDirection)(dir)
    } yield (command, 1.0)).toMap
  }

  def minPathSensor(lab: Labyrinth, from: Point,
                    robotDirection: Direction, goal: Point): CommandSignal =
    minPathSensor(lab, from, robotDirection, costMap(lab, goal))
}
