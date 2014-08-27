package geneticmachine.labyrinth

package object vision {
  trait Vision extends Serializable {
    def apply(labyrinth: Labyrinth, from: Point): Observation
  }

  final case class Observation(visionMap: Labyrinth, from: Point) {
    def impose(lab: Labyrinth): Labyrinth = {
      val depth = (visionMap.rows - 1) / 2
      for {
        x <- 0 until visionMap.rows
        y <- 0 until visionMap.cols
        labX = x + from.x - depth
        labY = y + from.y - depth
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

  type Pattern = (Observation) => Double
}
