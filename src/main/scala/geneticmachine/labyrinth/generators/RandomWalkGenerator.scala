package geneticmachine.labyrinth.generators

import geneticmachine.labyrinth._

import scala.util.Random

final case class RandomWalkGenerator(lineLenDelta: Int, minLineLen: Int)
                                    (size: Point) extends LabyrinthGenerator {

  override def toString: String = s"RandomWalkGenerator(line: $minLineLen - ${minLineLen + lineLenDelta}, " +
    s"size: ${size.x}x${size.y})"

  def apply(): (Labyrinth, RobotPosition, Point) = {
    val Point(rows, cols) = size
    val lab = Labyrinth.occupied(rows, cols)

    val start = Point(0, (cols - 1) / 2)
    val goal = Point(rows - 1, (cols - 1)/ 2)

    def gen(lab: Labyrinth, p: Point, goal: Point): Labyrinth = {
      if (p != goal) {
        val len = Random.nextInt(lineLenDelta) + minLineLen
        val direction = Direction.directions(Random.nextInt(4))
        val newP = line(lab, p, len, direction)
        gen(lab, newP, goal)
      } else {
        lab
      }
    }

    (gen(lab, start, goal), RobotPosition(start, Direction.North), goal)
  }
}
