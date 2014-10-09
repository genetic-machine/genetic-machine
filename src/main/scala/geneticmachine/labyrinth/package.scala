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
    final val startPosition: CellStatus = -2
    final val goal: CellStatus = -3
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

    def reverse: Point = Point(-x, -y)

    /** But it is the complex multiplication, i.e. rotation. **/
    def *(other: Point) = Point(this.x * other.x - this.y * other.y,
      this.y * other.x + this.x * other.y)

    def *(scale: Int) = Point(this.x * scale, this.y * scale)

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

  type Path = List[RobotPosition]

  object LabyrinthCommand extends Enumeration {
    type LabyrinthCommand = Value
    val TurnLeft, TurnRight, Forward = Value
  }

  object Direction {

    final val North = Point(1, 0)
    final val South = Point(-1, 0)
    final val West = Point(0, 1)
    final val East = Point(0, -1)
    final val Zero = Point(0, 0)

    final val directions = Seq(North, South, West, East)

    /**
     * Only for nice serialization.
     */
    final def id(dir: Direction): Char = dir match {
      case `North` => 'V'
      case `South` => '^'
      case `West` => '>'
      case `East` => '<'
      case _ => '?'
    }

    final def fromId(dirC: Char): Direction = dirC match {
      case 'V' => North
      case '^' => South
      case '>' => West
      case '<' => East
      case _ => Zero
    }

    type Direction = Point
  }

  import Direction._

  /**
   * General BFS algorithm.
   * @param depth maximal depth
   * @param neighbor generates states reachable from given point.
   *                 Cost between current and produced states is assumed to be 1.
   * @tparam A type of state
   * @return cost map: state -> cost
   */
  def breadthFirstSearch[A](from: A, depth: Int = Int.MaxValue)(neighbor: A => Seq[A]): Map[A, Int] = {
    def bfs(open: Set[A], depth: Int, cost: Map[A, Int]): Map[A, Int] = {
      val wave = (for {
        state <- open
        n <- neighbor(state)
        if !cost.contains(n) || cost(n) > cost(state) + 1
      } yield (n, cost(state) + 1)).toMap

      if (wave.nonEmpty && depth > 0) {
        bfs(wave.keySet, depth - 1, cost ++ wave)
      } else {
        cost
      }
    }
    bfs(Set[A](from), depth, Map[A, Int](from -> 0))
  }

  /**
   * Potential map of labyrinth without respect to robot's direction.
   * Optimized bfs.
   */
  def costMapWithoutDirection(lab: Labyrinth, from: Point, depth: Int = Int.MaxValue): DenseMatrix[Int] = {
    val costMap = DenseMatrix.fill(lab.rows, lab.cols)(Int.MaxValue)
    costMap(from.x, from.y) = 0

    def bfs(openSet: Set[Point], closedSet: Set[Point], depth: Int) {
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
        bfs(newOpenSet, closedSet.union(openSet), depth - 1)
      }
    }

    bfs(Set[Point](from), Set.empty[Point], depth)
    costMap
  }

  final case class RobotPosition(point: Point, direction: Direction) {

    def forward = RobotPosition(point + direction, direction)

    def turnLeft = RobotPosition(point, direction.turnLeft)

    def turnRight = RobotPosition(point, direction.turnRight)

    def reverse = RobotPosition(point, direction.reverse)

    def neighbors = List(forward, turnLeft, turnRight)

    /** Returns true only for [[Zero]] and [[Free]] cells of `lab`. **/
    def inLabyrinth(lab: Labyrinth): Boolean = point.inLabyrinth(lab)

    def *(command: LabyrinthCommand.LabyrinthCommand): RobotPosition = {
      command match {
        case LabyrinthCommand.Forward => forward
        case LabyrinthCommand.TurnLeft => turnLeft
        case LabyrinthCommand.TurnRight => turnRight
      }
    }

    def /(other: RobotPosition): Option[LabyrinthCommand.LabyrinthCommand] = {
      if (other == this.forward) {
        Some(LabyrinthCommand.Forward)
      } else if (other == this.turnLeft) {
        Some(LabyrinthCommand.TurnLeft)
      } else if (other == this.turnRight) {
        Some(LabyrinthCommand.TurnRight)
      } else {
        None
      }
    }

    def apply(command: LabyrinthCommand.LabyrinthCommand) = this * command

    def %%(lab: Labyrinth): Boolean = this.inLabyrinth(lab)

    def neighborsInLabyrinth(lab: Labyrinth): Seq[RobotPosition] = neighbors.filter { rp =>
      rp.point.inLabyrinth(lab)
    }

    def actionOpt(lab: Labyrinth)(command: LabyrinthCommand.LabyrinthCommand): Option[RobotPosition] = {
      if ((this * command) %% lab) {
        Some(this * command)
      } else {
        None
      }
    }

    def action(lab: Labyrinth)(command: LabyrinthCommand.LabyrinthCommand): RobotPosition = {
      actionOpt(lab)(command).getOrElse(this)
    }

    def actions(lab: Labyrinth): Seq[(LabyrinthCommand.LabyrinthCommand, RobotPosition)] = {
      Seq (
        (LabyrinthCommand.TurnLeft, turnLeft),
        (LabyrinthCommand.Forward, forward),
        (LabyrinthCommand.TurnRight, turnRight)
      ).filter { actionPos =>
        actionPos._2.inLabyrinth(lab)
      }
    }
  }

  /**
   * Potential map of labyrinth with respect to robot's direction.
   */
  def costMap(lab: Labyrinth, from: RobotPosition, depth: Int = Int.MaxValue): DenseMatrix[Int] = {
    val cost = costDict(lab, from, depth)

    val potentialMap = DenseMatrix.fill[Int](lab.rows, lab.cols)(Int.MaxValue)
    for ((rp, value) <- cost) {
      potentialMap(rp.point.x, rp.point.y) = potentialMap(rp.point.x, rp.point.y) min value
    }

    potentialMap
  }

  def reverseCostMap(lab: Labyrinth, goal: Point): DenseMatrix[Int] = {

    def matrixMin(ms: Seq[DenseMatrix[Int]]): DenseMatrix[Int] = {
      val acc = ms(0).copy

      for (m <- ms.tail) {
        for (i <- 0 until acc.rows; j <- 0 until acc.cols) {
          acc(i, j) = acc(i, j) min m(i, j)
        }
      }

      acc
    }

    matrixMin {
      directions.map { dir =>
        costMap(lab, RobotPosition(goal, dir))
      }
    }
  }

  type CostDict = Map[RobotPosition, Int]

  def costDict(lab: Labyrinth, from: RobotPosition, depth: Int = Int.MaxValue): CostDict = {
    breadthFirstSearch[RobotPosition](from, depth) { rp: RobotPosition =>
      rp.neighborsInLabyrinth(lab)
    }
  }

  /**
   * Cost dict from goal to all possible positions. Since goal isn't [[RobotPosition]] i.e.
   * doesn't contain [[Direction.Direction]], this method takes minimum for each cell
   * for all possible directions in goal position.
   */
  def reverseCostDict(lab: Labyrinth, goal: Point): CostDict = {
    directions.map { dir =>
      breadthFirstSearch[RobotPosition](RobotPosition(goal, dir)) { rp: RobotPosition =>
        rp.neighborsInLabyrinth(lab).map { n: RobotPosition => n.reverse }
      }
    }.reduceLeft { (d1: CostDict, d2: CostDict) =>
      (for {
        (rp, cost1) <- d1
        cost2 = d2(rp)
      } yield (rp, cost1 min cost2)).toMap
    }
  }

  def costDictToMap(cost: CostDict): CostMap = {
    val maxX = cost.keys.maxBy { rp: RobotPosition => rp.point.x }.point.x
    val maxY = cost.keys.maxBy { rp: RobotPosition => rp.point.y }.point.y

    val matrix = DenseMatrix.fill(maxX + 1, maxY + 1)(Int.MaxValue)
    for ((rp, v) <- cost) {
      matrix(rp.point.x, rp.point.y) = v min matrix(rp.point.x, rp.point.y)
    }

    matrix
  }

  def toCharMatrix(lab: Labyrinth): DenseMatrix[Char] = {
    lab map {
      case `Free` => ' '
      case `Occupied` => '#'
      case `Unknown` => '`'
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

  def printLab(lab: Labyrinth): String = {
    labToString(toCharMatrix(lab))
  }

  type CommandSignal = Map[LabyrinthCommand.LabyrinthCommand, Double]

  object CommandSignal {
    def empty = Map (
      LabyrinthCommand.Forward -> 0.0,
      LabyrinthCommand.TurnLeft -> 0.0,
      LabyrinthCommand.TurnRight -> 0.0
    )

    def normed(cs: CommandSignal): CommandSignal = {
      val statSum = cs.values.sum
      if (statSum > 0.0) {
        for {
          (k, v) <- cs
        } yield (k, v / statSum)
      } else {
        cs
      }
    }

    def apply(cs: CommandSignal) = empty ++ cs
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

  case class LabyrinthInput(lab: Labyrinth, robotPosition: RobotPosition, goal: Point) {
    def observation: Observation = Observation(lab, robotPosition).orientated
  }

  case class LabyrinthState(visionMap: Labyrinth, labyrinth: Labyrinth,
                            robotPosition: RobotPosition,
                            goal: Point, path: Path, history: List[LabyrinthCommand.LabyrinthCommand]) {

    def toCharMap: DenseMatrix[Char] = {
      val result = toCharMatrix(visionMap)

      path.foreach { p =>
        result(p.point.x, p.point.y) = if (result(p.point.x, p.point.y) == '+') { '*' } else { '+' }
      }

      result(robotPosition.point.x, robotPosition.point.y) = Direction.id(robotPosition.direction)

      result
    }

    override def toString: String = {
      s"LabyrinthStatus(labyrinth: ${labyrinth.rows}x${labyrinth.cols}, " +
        s"position: $robotPosition, direction, goal: $goal, history: ${history.size} commands)"
    }

    def printVision: String = {
      val vis = toCharMatrix(visionMap)
      for (p <- path) {
        vis(p.point.x, p.point.y) = '.'
      }

      vis(robotPosition.point.x, robotPosition.point.y) = Direction.id(robotPosition.direction)

      labToString(vis)
    }
  }

  trait LabyrinthHeuristic extends (LabyrinthInput => CommandSignal) {
    override def toString: String = "Incomplete Labyrinth Heuristic"

    final def optimalCommand(input: LabyrinthInput): LabyrinthCommand.LabyrinthCommand = {
      this(input).maxBy {
        kv: (LabyrinthCommand.LabyrinthCommand, Double) => kv._2
      }._1
    }
  }

  type LabyrinthOutput = LabyrinthCommand.LabyrinthCommand

  case class LabyrinthFeedback(value: Double) {
    final def +(other: LabyrinthFeedback) = LabyrinthFeedback(this.value + other.value)

    final def +(other: Double) = LabyrinthFeedback(this.value + other)
  }

  abstract class LabyrinthBrain[StateT : ClassTag](dff: DataFlowFormat) extends
    Brain[LabyrinthInput, LabyrinthOutput, LabyrinthFeedback, StateT](dff)

  abstract class LabyrinthBrainFactory extends
    BrainFactory[LabyrinthInput, LabyrinthOutput, LabyrinthFeedback]
}
