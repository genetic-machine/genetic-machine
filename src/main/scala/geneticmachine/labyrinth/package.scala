package geneticmachine

import breeze.linalg.DenseMatrix
import common.dataflow.DataFlowFormat
import scala.math._
import scala.Ordering
import scala.reflect.ClassTag
import geneticmachine.labyrinth.vision._

package object labyrinth {

  type CellStatus = Int

  object CellStatus {
    final val Free: CellStatus = -1
    final val Occupied: CellStatus = 1
    final val Unknown: CellStatus = 0
  }

  import CellStatus._

  type Labyrinth = DenseMatrix[CellStatus]

  def matrixToArray[A : ClassTag](m: DenseMatrix[A]): (Array[A], Int, Int) = {
    val flatMatrix = for {
      row <- 0 until m.rows
      col <- 0 until m.cols
    } yield m(row, col)

    (flatMatrix.toArray, m.rows, m.cols)
  }

  def matrixFromArray[A](array: Array[A], rows: Int, cols: Int): DenseMatrix[A] = {
    new DenseMatrix[A](rows, cols, array, 0)
  }

  object Labyrinth {
    def apply(rows: Int, cols: Int) = unknown(rows, cols)
    def unknown(rows: Int, cols: Int): Labyrinth = DenseMatrix.fill[Int](rows, cols)(Unknown)
    def occupied(rows: Int, cols: Int): Labyrinth = DenseMatrix.fill[Int](rows, cols)(Occupied)
    def free(rows: Int, cols: Int): Labyrinth = DenseMatrix.fill[Int](rows, cols)(Free)

    def toArray(lab: Labyrinth): (Array[CellStatus], Int, Int) = matrixToArray(lab)
  }

  type CostMap = DenseMatrix[Int]

  /**
   * It is not a complex value, it represents 2D vector.
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

    /** But it is the complex multiplication, i.e. rotation. **/
    def *(other: Point) = Point(this.x * other.x - this.y * other.y,
      this.y * other.x + this.x * other.y)

    def *(scale: Int) = Point(this.x * scale, this.x * scale)

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

    /** Labyrinth operators **/
    def inBorders(sizeX: Int, sizeY: Int): Boolean = (x >= 0) && (x < sizeX) && (y >= 0) && (y < sizeY)

    def inLabyrinth(lab: Labyrinth): Boolean = inBorders(lab.rows, lab.cols) && (lab(x, y) != Occupied)

    def neighborsInLabyrinth(lab: Labyrinth) = neighbors filter { _.inLabyrinth(lab) }

    /** 'Crop' operator */
    def %(lab: Labyrinth): Point = {
      val x_ = (0 max x) min (lab.rows - 1)
      val y_ = (0 max y) min (lab.cols - 1)
      Point(x_, y_)
    }
  }

  object Point {
    val zero = Point(0, 0)
  }

  type Path = List[Point]

  object LabyrinthCommand extends Enumeration {
    type LabyrinthCommand = Value
    val TurnLeft, TurnRight, Forward = Value
  }

  type CommandSignal = Map[LabyrinthCommand.LabyrinthCommand, Double]

  object CommandSignal {
    def empty = Map (
      LabyrinthCommand.Forward -> 0.0,
      LabyrinthCommand.TurnLeft -> 0.0,
      LabyrinthCommand.TurnRight -> 0.0
    )

    def apply(cs: CommandSignal) = empty ++ cs
  }

  object Direction {

    final val North = Point(1, 0)
    final val South = Point(-1, 0)
    final val West = Point(0, 1)
    final val East = Point(0, -1)

    final val directions = Seq(North, South, West, East)

    type Direction = Point
  }

  import Direction._

  def costMap(lab: Labyrinth, from: Point, depth: Int = Int.MaxValue): DenseMatrix[Int] = {
    val costMap = DenseMatrix.fill(lab.rows, lab.cols)(Int.MaxValue)
    costMap(from.x, from.y) = 0

    def breadthFirstSearch(openSet: Set[Point], closedSet: Set[Point], depth: Int) {
      val newOpenSet = for {
        p <- openSet
        n <- p.neighborsInLabyrinth(lab)
        if !closedSet.contains(n)
        if costMap(n.x, n.y) > costMap(p.x, p.y) + 1
      } yield {
        costMap(n.x, n.y) = costMap(p.x, p.y) + 1
        n
      }

      if (newOpenSet.nonEmpty && depth > 0) {
        breadthFirstSearch(newOpenSet, closedSet.union(openSet), depth - 1)
      }
    }

    breadthFirstSearch(Set[Point](from), Set.empty[Point], depth)
    costMap
  }



  def printLab(lab: Labyrinth): DenseMatrix[Char] = {
    lab map {
      case Free => ' '
      case Occupied => '#'
      case Unknown => '`'
    }
  }

  def labToString(lab: DenseMatrix[Char]): String = {
    val seqMatrix = for {
      x <- 0 until lab.rows
    } yield for {
        y <- 0 until lab.cols
      } yield lab(x, y)

    seqMatrix.map { _.mkString("") }.mkString("\n")
  }

  def directionToCommand(robotDirection: Direction)(direction: Direction): Seq[LabyrinthCommand.LabyrinthCommand] = {
    val robotDirectionLeft = robotDirection.turnLeft
    val robotDirectionRight = robotDirection.turnRight
    direction match {
      case `robotDirection` => Seq(LabyrinthCommand.Forward)
      case `robotDirectionLeft` => Seq(LabyrinthCommand.TurnLeft)
      case `robotDirectionRight` => Seq(LabyrinthCommand.TurnRight)
      case _ => Seq(LabyrinthCommand.TurnLeft, LabyrinthCommand.TurnRight)
    }
  }

  def strictMinPathSensor(lab: Labyrinth, from: Point,
                          robotDirection: Direction,
                          cost: CostMap): CommandSignal = {

    val localCost = for {
      dir <- directions
      p = from + dir
      if p.inLabyrinth(lab)
      c = cost(p.x, p.y) + (dir - robotDirection).lInfNorm
    } yield (dir, c)

    val (_, minCost) = localCost min (Ordering by { x: (Direction, Int) => x._2 })
    val minDirs = localCost filter { _._2 == minCost } map { _._1 }

    CommandSignal {
      (for {
        dir <- minDirs
        command <- directionToCommand(robotDirection)(dir)
      } yield (command, 1.0)).toMap
    }
  }

  case class LabyrinthInput(lab: Labyrinth, robotPosition: Point, robotDirection: Direction, goal: Point) {
    def observation: Observation = Observation(lab, robotPosition).turn(robotDirection)
  }

  case class LabyrinthState(visionMap: Labyrinth, labyrinth: Labyrinth,
                             robotPosition: Point, robotDirection: Direction.Direction,
                             goal: Point, path: Path, history: List[LabyrinthCommand.LabyrinthCommand]) {

    def toCharMap: DenseMatrix[Char] = {
      val result = printLab(visionMap)

      path.foreach { p =>
        result(p.x, p.y) = '+'
      }

      result(robotPosition.x, robotPosition.y) = robotDirection match {
        case Direction.North => 'V'
        case Direction.South => 'A'
        case Direction.West => '>'
        case Direction.East => '<'
      }

      result
    }

    override def toString: String = {
      s"LabyrinthStatus(labyrinth: ${labyrinth.rows}x${labyrinth.cols}, " +
        s"position: $robotPosition, direction: $robotDirection, goal: $goal, history: ${history.size} commands)"
    }
  }

  def strictMinPathSensor(labInput: LabyrinthInput): CommandSignal = {
    val LabyrinthInput(lab, from, robotDirection, goal) = labInput
    strictMinPathSensor(lab, from, robotDirection, costMap(lab, goal))
  }

  type LabyrinthOutput = LabyrinthCommand.LabyrinthCommand

  case class LabyrinthFeedback(value: Double)

  abstract class LabyrinthBrain[StateT : ClassTag](dff: DataFlowFormat) extends
    Brain[LabyrinthInput, LabyrinthOutput, LabyrinthFeedback, StateT](dff)
}
