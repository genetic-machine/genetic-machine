package geneticmachine.labyrinth

import geneticmachine.labyrinth.Direction._

package object generators {
  trait LabyrinthGenerator extends Serializable {
    def apply(): (Labyrinth, Point, Point)
  }

  def line(lab: Labyrinth, p: Point, len: Int, direction: Direction): Point = {
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
}
