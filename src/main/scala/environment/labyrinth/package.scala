package environment

import breeze.linalg.DenseMatrix
import scala.util.Random
import scala.math.{ abs,sqrt }
import scala.reflect.ClassTag
import breeze.storage.DefaultArrayValue

package object labyrinth {

  object CellStatus extends Enumeration{
    type CellStatus = Value
    val Free, Occupied, Unknown = Value
  }

  import CellStatus._

  type Labyrinth = DenseMatrix[CellStatus]
  type CostMap = DenseMatrix[Int]

  final case class Point(x: Int, y: Int) {

    def neighbors: List[Point] = left :: right :: backward :: forward :: Nil

    def left: Point =  Point(x - 1, y)

    def right: Point = Point(x + 1, y)

    def backward: Point = Point(x, y - 1)

    def forward: Point = Point(x, y + 1)

    def +(other: Point) = Point(this.x + other.x, this.y + other.y)
    def -(other: Point) = Point(this.x - other.x, this.y - other.y)
    def *(other: Point) = Point(this.x * other.x - this.y * other.y,
                                this.y * other.x + this.x * other.y)

    def adjoint: Point = Point(x, -y)

    def inBorders(sizeX: Int, sizeY: Int): Boolean = (x >= 0) && (x < sizeX) && (y >= 0) && (y < sizeY)

    def inLabyrinth(lab: Labyrinth): Boolean = inBorders(lab.rows, lab.cols) && (lab(x, y) != Occupied)

    def neighborsInLabyrinth(lab: Labyrinth) = neighbors filter { _.inLabyrinth(lab) }

    def lInfNorm = abs(x) max abs(y)
    def l1Norm = abs(x) + abs(y)
  }

  type Path = List[Point]

  def costMap(lab: Labyrinth, from: Point, deep: Int = Int.MaxValue) : DenseMatrix[Int] = {
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

      if (!newOpenSet.isEmpty && deep > 0) {
        deepFirstSearch(newOpenSet, closedSet.union(openSet), deep - 1)
      }
    }

    deepFirstSearch(Set[Point](from), Set.empty[Point], deep)
    costMap
  }

  def visionByPattern(lab: Labyrinth, costPattern: CostMap, from: Point): Labyrinth = {
    val vision = DenseMatrix.fill(costPattern.rows, costPattern.cols)(Occupied)
    val cost = costMap(lab, from)
    val size: Int = (costPattern.cols - 1) / 2

    for {
      x <- 0 until costPattern.rows
      y <- 0 until costPattern.cols
      labX: Int = from.x + x - size
      labY: Int = from.y + y - size
      if (labX >= 0) && (labX < lab.rows)
      if (labY >= 0) && (labY < lab.rows)
    } {
      vision(x, y) = if (cost(labX, labY) <= costPattern(x, y) + 0.1) Free else Occupied
    }

    vision
  }

  object ManhattanVision {
    def visionPattern(deep: Int): CostMap = {
      val size = deep * 2 + 1
      val pattern = DenseMatrix.fill[Int](size, size)(0)

      for {
        x <- 0 until size
        y <- 0 until size
      } {
        val pointDeep = abs(x - deep) + abs(y - deep)
        pattern(x, y) = pointDeep min deep
      }

      pattern
    }

    def vision(lab: Labyrinth, from: Point, deep: Int): Labyrinth = {
      visionByPattern(lab, visionPattern(deep), from)
    }

    def printVision(lab: Labyrinth, from: Point, deep: Int): DenseMatrix[Char] = {
      val picture = printLab(lab)
      val vis = vision(lab, from, deep)

      for {
        x <- 0 until lab.rows
        y <- 0 until lab.cols
        visX = from.x - deep + x
        visY = from.y - deep + y
        if visX >= 0 && visX < vis.rows
        if visY >= 0 && visY < vis.cols
      } {
        if (vis(visX, visY) == Free) picture(x, y) = '`'
      }

      picture(from.x, from.y) = 'O'
      picture
    }
  }

  object EuclideanVision {
    val sqrt2 = sqrt(2.0)

    def distanceNeighbors(p1: Point, p2: Point): Double = {
      val Point(dx, dy) = p1 - p2
      val diff = abs(dx) + abs(dy)
      if (diff == 2) sqrt2 else 1.0
    }

    def costMap(lab: Labyrinth, from: Point, deep: Double): DenseMatrix[Double] = {
      val costMap = DenseMatrix.fill(lab.rows, lab.cols)(Double.MaxValue)
      costMap(from.x, from.y) = 0.0

      def neighbors(p: Point): Seq[Point] = {
        val Point(x, y) = p
        for {
          dx: Int <- -1 to 1
          dy: Int <- -1 to 1
          p = Point(x + dx, y + dy)
          if p.inLabyrinth(lab)
        } yield p
      }

      def deepFirstSearch(openSet: Set[Point], closedSet: Set[Point]) {
        val newOpenSet = for {
          p <- openSet
          n <- neighbors(p)
          newDist = costMap(p.x, p.y) + distanceNeighbors(n, p)
          if !closedSet.contains(n)
          if newDist <= deep
          if costMap(n.x, n.y) > newDist
        } yield {
          costMap(n.x, n.y) = newDist
          n
        }

        if (!newOpenSet.isEmpty) {
          deepFirstSearch(newOpenSet, closedSet.union(openSet))
        }
      }

      deepFirstSearch(Set[Point](from), Set.empty[Point])
      costMap
    }

    def visionPattern(deep: Int): DenseMatrix[Double] = {
      val size = 2 * deep + 1
      val freeMap = DenseMatrix.fill(size, size)(Free)
      val cost = costMap(freeMap, Point(deep, deep), deep.toDouble + 0.1)

      cost.map { c => if (c < deep.toDouble + 0.1) c else -1.0 }
    }

    def visionByPattern(lab: Labyrinth, costPattern: DenseMatrix[Double], from: Point): Labyrinth = {
      val vision = DenseMatrix.fill(costPattern.rows, costPattern.cols)(Occupied)
      val cost = costMap(lab, from, Double.MaxValue)
      val size: Int = (costPattern.cols - 1) / 2

      for {
        x <- 0 until costPattern.rows
        y <- 0 until costPattern.cols
        labX: Int = from.x + x - size
        labY: Int = from.y + y - size
        if (labX >= 0) && (labX < lab.rows)
        if (labY >= 0) && (labY < lab.rows)
      } {
        vision(x, y) = if (cost(labX, labY) <= costPattern(x, y)) Free else Occupied
      }

      vision
    }

    def vision(lab: Labyrinth, from: Point, deep: Int): Labyrinth = {
      visionByPattern(lab, visionPattern(deep), from)
    }

    def printVision(lab: Labyrinth, from: Point, deep: Int): DenseMatrix[Char] = {
      val picture = printLab(lab)
      val vis = vision(lab, from, deep)

      for {
        x <- 0 until lab.rows
        y <- 0 until lab.cols
        visX = from.x - deep + x
        visY = from.y - deep + y
        if visX >= 0 && visX < vis.rows
        if visY >= 0 && visY < vis.cols
      } {
        if (vis(visX, visY) == Free) picture(x, y) = '`'
      }

      picture(from.x, from.y) = 'O'
      picture
    }
  }

  object FullVision {
    def vision(lab: Labyrinth, from: Point, deep: Int): Labyrinth = {
      val size = 2 * deep + 1
      val visionField = DenseMatrix.fill(size, size)(Unknown)

      for {
        x <- 0 until size
        y <- 0 until size
        labX = x - deep + from.x
        labY = y - deep + from.y
        if abs(x - deep) + abs(y - deep) > deep
      } {
        visionField(x, y) = if (Point(labX, labY).inBorders(lab.rows, lab.cols)) lab(labX, labY) else Occupied
      }

      visionField
    }
  }

  def simpleLabyrinthGen(sizeX: Int, sizeY: Int): Labyrinth = {
    val lab = DenseMatrix.fill(sizeX, sizeY)(Occupied)

    val goal = Point(sizeX - 1, (sizeY + 1) / 2)
    val start = Point(0, (sizeY + 1) / 2)

    def forward(p: Point, len: Int, dirX: Int, dirY: Int): Point = {
      if (len > 0) {
        lab(p.x, p.y) = Free
        val modX = ((p.x + dirX) min (lab.rows - 1)) max 0
        val modY = ((p.y + dirY) min (lab.cols - 1)) max 0

        forward(Point(modX, modY), len - 1, dirX, dirY)
      } else {
        p
      }
    }

    def gen(lab: Labyrinth, p: Point, goal: Point): Labyrinth = {
      if (p != goal) {
        val len = Random.nextInt(5) + 3
        val direction = Random.nextInt(4)
        val (xDir, yDir) = direction match {
          case 0 => (1, 0)
          case 1 => (-1, 0)
          case 2 => (0, 1)
          case 3 => (0, -1)
        }

        val newP = forward(p, len, xDir, yDir)
        gen(lab, newP, goal)
      } else {
        lab
      }
    }

    gen(lab, start, goal)
  }

  def printLab(lab: Labyrinth): DenseMatrix[Char] = {
    lab map { c =>
      if (c == Free) ' ' else 'X'
    }
  }
}
