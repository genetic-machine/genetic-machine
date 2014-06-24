package geneticmachine.labyrinth.vision

import geneticmachine.labyrinth._

final case class Observation(visionMap: Labyrinth, from: Point) {
  def impose(lab: Labyrinth): Labyrinth = {
    val deep = (visionMap.rows - 1) / 2
    for {
      x <- 0 until visionMap.rows
      y <- 0 until visionMap.cols
      labX = x + from.x - deep
      labY = y + from.y - deep
      if labX >= 0
      if labY >= 0
      if labX < lab.rows
      if labY < lab.cols
      if visionMap(x, y) != CellStatus.Unknown
    } {
      lab(labX, labY) = visionMap(x, y)
    }

    lab
  }
}

trait Vision {
  def apply(labyrinth: Labyrinth, from: Point): Observation
}
