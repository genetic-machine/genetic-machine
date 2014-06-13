package robot.labyrinth.generators

import robot.labyrinth.Direction._
import robot.labyrinth._

import scala.util.Random

object RandomLineGenerator {
  def apply(lineLenDelta: Int, minLineLen: Int)(rows: Int, cols: Int) =
    new RandomLineGenerator(lineLenDelta, minLineLen)(rows, cols)
}

final class RandomLineGenerator(val lineLenDelta: Int, val minLineLen: Int)
                               (val rows: Int, val cols: Int) extends LabyrinthGenerator {

  def apply(): Labyrinth = {
    val lab = Labyrinth.occupied(rows, cols)

    val start = Point(0, (cols - 1) / 2)
    val goal = Point(rows - 1, (cols - 1)/ 2)

    def line(p: Point, len: Int, direction: Direction): Point = {
      val len_ = (direction match {
        case Direction.North => lab.rows - 1 - p.x
        case Direction.South => p.x
        case Direction.West => lab.cols - 1 - p.y
        case Direction.East => p.y
      }) min len

      (0 until len_).toList.scanLeft(p) { (l, _) =>
        l + direction
      }.foldLeft(Point.zero) { (_, l) =>
        lab(l.x, l.y) = CellStatus.Free
        l
      }
    }

    def gen(lab: Labyrinth, p: Point, goal: Point): Labyrinth = {
      if (p != goal) {
        val len = Random.nextInt(lineLenDelta) + minLineLen
        val direction = Direction.directions(Random.nextInt(4))
        val newP = line(p, len, direction)
        gen(lab, newP, goal)
      } else {
        lab
      }
    }

    gen(lab, start, goal)
  }
}
