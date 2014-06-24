package geneticmachine.labyrinth.generators

import geneticmachine.labyrinth.Direction._
import geneticmachine.labyrinth._

import scala.util.Random

object RandomWalkGenerator {
  def apply(lineLenDelta: Int, minLineLen: Int)(rows: Int, cols: Int) =
    new RandomWalkGenerator(lineLenDelta, minLineLen)(rows, cols)
}

final class RandomWalkGenerator(val lineLenDelta: Int, val minLineLen: Int)
                               (val rows: Int, val cols: Int) extends LabyrinthGenerator {

  def apply(): (Labyrinth, Point, Point) = {
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

    (gen(lab, start, goal), start, goal)
  }
}
