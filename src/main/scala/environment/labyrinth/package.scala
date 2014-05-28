package environment

import breeze.linalg.DenseMatrix
import scala.util.Random

package object labyrinth {

  object CellStatus extends Enumeration{
    type CellStatus = Value
    val Free, Occupied, Unknown = Value
  }

  import CellStatus._

  type Labyrinth = DenseMatrix[CellStatus]

  final case class Point(x: Int, y: Int) {

    def neighbors: List[Point] = left :: right :: backward :: forward :: Nil

    def left: Point =  Point(x - 1, y)

    def right: Point = Point(x + 1, y)

    def backward: Point = Point(x, y - 1)

    def forward: Point = Point(x, y + 1)

    def +(other: Point) = Point(this.x + other.x, this.y + other.y)
    def -(other: Point) = Point(this.x - other.x, this.y - other.y)

    def inBorders(sizeX: Int, sizeY: Int): Boolean = (x >= 0) && (x < sizeX) && (y >= 0) && (y < sizeY)

    def inLabyrinth(lab: Labyrinth): Boolean = inBorders(lab.rows, lab.cols) && (lab(x, y) == Free)

    def neighborsInLabyrinth(lab: Labyrinth) = neighbors filter { _.inLabyrinth(lab) }
  }

  type Path = List[Point]

  def costMap(lab: Labyrinth, goal: Point): DenseMatrix[Int] = {
    val costMap = DenseMatrix.fill(lab.rows, lab.cols)(Int.MaxValue)
    costMap(goal.x, goal.y) = 0

    def deepFirstSearch(openSet: Set[Point], closedSet: Set[Point]) {
      val newOpenSet = for {
        p <- openSet
        n <- p.neighborsInLabyrinth(lab)
        if !closedSet.contains(n)
        if costMap(n.x, n.y) > costMap(p.x, p.y) + 1
      } yield {
        costMap(n.x, n.y) = costMap(p.x, p.y) + 1
        n
      }

      if (!newOpenSet.isEmpty) {
        deepFirstSearch(newOpenSet, closedSet.union(openSet))
      }
    }

    deepFirstSearch(Set[Point](goal), Set.empty[Point])
    costMap
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

  def printLab(lab: Labyrinth): String = {
    (for {
      x <- 0 until lab.rows
    } yield (for {
      y <- 0 until lab.cols
    } yield if (lab(x, y) == Occupied) '*' else ' ').mkString(" ")).mkString("\n")
  }
}
