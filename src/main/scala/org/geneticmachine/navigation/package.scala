package org.geneticmachine

import breeze.linalg._
import breeze.storage.Zero
import scala.collection.mutable.ArrayBuffer
import scala.reflect.ClassTag

import org.geneticmachine.navigation.vision._

package object navigation {
  type CellStatus = Int

  import CellStatus._

  type Labyrinth = DenseMatrix[CellStatus]

  def matrixToArray[A : ClassTag](m: DenseMatrix[A]): (Array[A], Int, Int) = {
    (m.data, m.rows, m.cols)
  }

  def matrixFromArray[A](array: Array[A], rows: Int, cols: Int): DenseMatrix[A] = {
    new DenseMatrix[A](rows, cols, array, 0)
  }

  type CostMap = DenseMatrix[Int]

  type Path = List[RobotPosition]

  type NavigationCommand = Int

  object NavigationCommand {
    val TurnLeft: NavigationCommand = 0
    val Forward: NavigationCommand = 1
    val TurnRight: NavigationCommand = 2

    val max: Int = 3

    def char(c: NavigationCommand): String = c match{
      case TurnLeft => "TurnLeft"
      case Forward => "Forward"
      case TurnRight => "TurnRight"
      case _ => "Unknown Command"
    }
  }

  type Direction = Point

  object Direction {

    final val Zero = Point(0, 0)

    final val North = Point(1, 0)
    final val South = Point(-1, 0)
    final val West = Point(0, 1)
    final val East = Point(0, -1)

    final val directions = Seq(North, South, West, East)

    /**
     * Only for nice serialization.
     */
    def char(dir: Direction): Char = dir match {
      case `North` => 'V'
      case `South` => '^'
      case `West` => '>'
      case `East` => '<'
      case _ => '?'
    }

    def fromChar(dirC: Char): Direction = dirC match {
      case 'V' => North
      case '^' => South
      case '>' => West
      case '<' => East
      case _ => Zero
    }

    def apply(dirC: Char): Direction = fromChar(dirC)

    private val dirToId = Map(North -> 0, East -> 1, South -> 2, West -> 3)
    private val idToDir = Array(North, East, South, West)

    def id(dir: Direction): Int = dirToId(dir)

    def apply(id: Int): Direction = idToDir(id)
  }

  import Direction._

  final case class RobotPosition(point: Point, direction: Direction) {

    override def toString: String = s"(${point.x}, ${point.y}, ${Direction.char(direction)})"

    def forward = RobotPosition(point + direction, direction)

    def backward = RobotPosition(point - direction, direction)

    def turnLeft = RobotPosition(point, direction.turnLeft)

    def turnRight = RobotPosition(point, direction.turnRight)

    def reverse = RobotPosition(point, direction.reverse)

    def neighbors = List(forward, turnLeft, turnRight)

    /** Returns true only for [[Zero]] and [[Free]] cells of `lab`. **/
    def inLabyrinth(lab: Labyrinth): Boolean = point.inLabyrinth(lab)

    def *(command: NavigationCommand): RobotPosition = {
      command match {
        case NavigationCommand.Forward => forward
        case NavigationCommand.TurnLeft => turnLeft
        case NavigationCommand.TurnRight => turnRight
      }
    }

    def /(other: RobotPosition): Option[NavigationCommand] = {
      if (other == this.forward) {
        Some(NavigationCommand.Forward)
      } else if (other == this.turnLeft) {
        Some(NavigationCommand.TurnLeft)
      } else if (other == this.turnRight) {
        Some(NavigationCommand.TurnRight)
      } else {
        None
      }
    }

    def apply(command: NavigationCommand) = this * command

    def %%(lab: Labyrinth): Boolean = this.inLabyrinth(lab)

    def neighborsInLabyrinth(lab: Labyrinth): Seq[RobotPosition] = neighbors.filter { rp =>
      rp.point.inLabyrinth(lab)
    }

    def actionOpt(lab: Labyrinth)(command: NavigationCommand): Option[RobotPosition] = {
      if ((this * command) %% lab) {
        Some(this * command)
      } else {
        None
      }
    }

    def action(lab: Labyrinth)(command: NavigationCommand): RobotPosition = {
      actionOpt(lab)(command).getOrElse(this)
    }

    def reverseActions(lab: Labyrinth): Seq[(NavigationCommand, RobotPosition)] = {
      import NavigationCommand._

      Seq( (TurnLeft, turnRight), (TurnRight, turnLeft), (Forward, backward) ).filter {
        case (action, rp) =>
          rp.inLabyrinth(lab)
      }
    }

    def actions(lab: Labyrinth): Seq[(NavigationCommand, RobotPosition)] = {
      import NavigationCommand._

      if (forward.inLabyrinth(lab)) {
        Seq( (TurnLeft, turnLeft), (Forward, forward), (TurnRight, turnRight) )
      } else {
        Seq( (TurnLeft, turnLeft), (TurnRight, turnRight) )
      }
    }
  }

  type CostDict = Array[DenseMatrix[Int]]

  def costF(costDict: CostDict)(rp: RobotPosition): Int = {
    costDict(Direction.id(rp.direction))(rp.point.x, rp.point.y)
  }

  def optimalAction(lab: Labyrinth, rp: RobotPosition, goal: Point): NavigationCommand = {
    val optCostDict: CostDict = reversedCostDict(lab, goal)
    optimalAction(lab, rp, goal, optCostDict)
  }

  def optimalAction(lab: Labyrinth, rp: RobotPosition, goal: Point, optCostDict: CostDict): NavigationCommand = {
    val cost = costF(optCostDict) _

    rp.actions(lab).map {
      case (act, p) => (act, cost(p))
    }.filter {
      case (act, c) => c > -1
    }.minBy {
      case (act, c) => c
    }._1
  }

  def reversedCostDict(lab: Labyrinth, goal: Point): CostDict = {
    val costs = Array.fill(4) {
      DenseMatrix.fill[Int](lab.rows, lab.cols)(-1)
    }

    reversedCostDict(lab, goal, costs)
  }

  def reversedCostDict(lab: Labyrinth, goal: Point, costs: Array[DenseMatrix[Int]]): CostDict = {
    type Direction = Int

    @inline
    def turnLeft(d: Direction): Direction = (d + 1) % 4

    @inline
    def turnRight(d: Direction): Direction = (d + 3) % 4

    @inline
    @unchecked
    def backward(x: Int, y: Int, d: Direction): (Int, Int) = d match {
      case 0 => (x - 1, y)
      case 1 => (x, y + 1)
      case 2 => (x + 1, y)
      case 3 => (x, y - 1)
    }

    @inline
    def check(x: Int, y: Int, d: Direction): Boolean = {
      if (x >= 0 && x < lab.rows && y >= 0 && y < lab.cols) {
        costs(d)(x, y) == -1 && lab(x, y) != CellStatus.Occupied
      } else {
        false
      }
    }

    def bfs(open: Array[Int], cost: Int): Unit = {
      val _open = new ArrayBuffer[Int](open.size)

      var i: Int = 0

      while (i < open.size) {
        val x: Int = open(i)
        val y: Int = open(i + 1)
        val d: Int = open(i + 2)
        i += 3


        val (bx, by) = backward(x, y, d)

        if (check(bx, by, d)) {
          costs(d)(bx, by) = cost

          _open += bx
          _open += by
          _open += d
        }

        val tl = turnRight(d)

        if (check(x, y, tl)) {
          costs(tl)(x, y) = cost

          _open += x
          _open += y
          _open += tl
        }

        val tr = turnLeft(d)

        if (check(x, y, tr)) {
          costs(tr)(x, y) = cost

          _open += x
          _open += y
          _open += tr
        }
      }

      if (_open.size > 0) {
        bfs(_open.toArray, cost + 1)
      }
    }

    val startOpen = (0 until 4).flatMap { i =>
      costs(i)(goal.x, goal.y) = 0
      Seq(goal.x, goal.y, i)
    }.toArray

    bfs(startOpen, 1)
    costs
  }

  def labToCharMatrix(lab: Labyrinth): DenseMatrix[Char] = {
    lab.map {
      case `Free` => ' '
      case `Occupied` => '#'
      case `Unknown` => '`'
    }
  }

  def matrixToString[T](lab: DenseMatrix[T]): String = {
    val seqMatrix = for {
      x <- 0 until lab.rows
    } yield for {
        y <- 0 until lab.cols
        c = lab(x, y)
      } yield s"$c$c"

    seqMatrix.map { _.mkString("") }.mkString("\n")
  }

  def printLab(lab: Labyrinth): String = {
    matrixToString(labToCharMatrix(lab))
  }

  val charSeq = Seq('#', '&', '*', '`', '-', '_', ' ')

  def printMatrixPattern(m: DenseMatrix[Double]): String = {
    val max = m.toArray.map(math.abs).max
    val s = (charSeq.size - 1) / 2

    val cm =m.map { c =>
      charSeq {
        ((c / max) * s).toInt + s
      }
    }

    matrixToString(cm)
  }

  def greyMap(_m: DenseMatrix[Double], normalize: Boolean = false): String = {
    val m = if (normalize) {
      val (min, max) = minMax(_m)
      (_m + min) / (max - min)
    } else {
      (_m.map { x =>
        (x max -1.0) min 1.0
      } + 1.0) / 2.0
    }

    val colors = (232 to 256).toArray

    val esc: Char = 27
    def color(x: Double): String = {
      val c = colors(((x * colors.size).ceil.toInt max 0) min (colors.size - 1))

      esc + "[48;5;" + c.toString + "m  " + esc + "[0m"
    }

    val seqMatrix = for {
      x <- 0 until m.rows
    } yield {
      for {
        y <- 0 until m.cols
      } yield m(x, y)
    }

    seqMatrix.map { row => row.map { x => color(x) }.mkString("") }.mkString("\n")
  }

  def heatMap(_m: DenseMatrix[Double]): String = {
    val z = max(_m.map { math.abs })
    val m = _m.map { _ / z }

    val redColors = 57.to(52, -1).toArray
    val blueColors = (16 to 21).toArray

    def red(x: Double): Int = {
      redColors((x * redColors.size).ceil.toInt min (redColors.size - 1))
    }

    def blue(x: Double): Int = {
      blueColors((-x * blueColors.size).ceil.toInt min (blueColors.size - 1))
    }

    val esc: Char = 27
    def color(x: Double): String = {
      val c = if (x > 0.0) {
        red(x)
      } else {
        blue(x)
      }

      esc + "[48;5;" + c.toString + "m  " + esc + "[0m"
    }

    val seqMatrix = for {
      x <- 0 until m.rows
    } yield {
      for {
        y <- 0 until m.cols
      } yield m(x, y)
    }

    seqMatrix.map { row => row.map { x => color(x) }.mkString("") }.mkString("\n")
  }

  type CommandSignal = Map[NavigationCommand, Double]

  object CommandSignal {
    def empty = Map (
      NavigationCommand.Forward -> 0.0,
      NavigationCommand.TurnLeft -> 0.0,
      NavigationCommand.TurnRight -> 0.0
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

  def directionToCommand(robotDirection: Direction)(direction: Direction): Seq[NavigationCommand] = {
    val robotDirectionLeft = robotDirection.turnLeft
    val robotDirectionRight = robotDirection.turnRight

    direction match {
      case `robotDirection` => Seq(NavigationCommand.Forward)
      case `robotDirectionLeft` => Seq(NavigationCommand.TurnLeft)
      case `robotDirectionRight` => Seq(NavigationCommand.TurnRight)
      case _ => Seq(NavigationCommand.TurnLeft, NavigationCommand.TurnRight)
    }
  }

  case class NavigationInput(lab: Labyrinth, robotPosition: RobotPosition, goal: Point, feedback: Double = 0.0) {
    def observation: VisionObservation  = VisionObservation(lab, robotPosition).orientated
  }

  final case class NavigationState(visionMap: Labyrinth, labyrinth: Labyrinth,
                                   robotPosition: RobotPosition,
                                   goal: Point, path: Path, history: List[NavigationCommand]) {

    def toCharMap: DenseMatrix[Char] = {
      val result = labToCharMatrix(visionMap)

      path.foreach { p =>
        result(p.point.x, p.point.y) = if (result(p.point.x, p.point.y) == '+') { '*' } else { '+' }
      }

      result(robotPosition.point.x, robotPosition.point.y) = Direction.char(robotPosition.direction)

      result
    }

    override def toString: String = {
      s"LabyrinthStatus(labyrinth: ${labyrinth.rows}x${labyrinth.cols}, " +
        s"position: $robotPosition, direction, goal: $goal, history: ${history.size} commands)"
    }

    def printVision: String = {
      val vis = labToCharMatrix(visionMap)
      for (p <- path) {
        vis(p.point.x, p.point.y) = '.'
      }

      vis(robotPosition.point.x, robotPosition.point.y) = Direction.char(robotPosition.direction)

      matrixToString(vis)
    }
  }

  type NavigationOutput = NavigationCommand

  type NavigationAlgorithm = Algorithm[NavigationInput, NavigationOutput]

  type NavigationAlgorithmGen[-C <: ExecutionContext] = AlgorithmGen[NavigationInput, NavigationOutput, C]
}
