package geneticmachine.labyrinth

package object vision {
  trait Vision extends Serializable {
    def apply(labyrinth: Labyrinth, from: RobotPosition): Observation
  }

  final case class Observation(visionMap: Labyrinth, from: RobotPosition) {
    def impose(lab: Labyrinth): Labyrinth = {
      val depth = (visionMap.rows - 1) / 2
      for {
        x <- 0 until visionMap.rows
        y <- 0 until visionMap.cols
        labX = x + from.point.x - depth
        labY = y + from.point.y - depth
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

    def turnNorth = this

    def turnEast: Observation = {
      val map = Labyrinth.unknown(visionMap.cols, visionMap.rows)
      for {
        row <- 0 until visionMap.rows
        col <- 0 until visionMap.cols
      } {
        map(col, row) = visionMap(row, col)
      }

      Observation(map, from.turnRight)
    }

    def turnWest: Observation = {
      val map = Labyrinth.unknown(visionMap.cols, visionMap.rows)
      for {
        row <- 0 until visionMap.rows
        col <- 0 until visionMap.cols
      } {
        map(visionMap.cols - col, row) = visionMap(row, col)
      }

      Observation(map, from.turnLeft)
    }

    def turnSouth: Observation = {
      val map = Labyrinth.unknown(visionMap.rows, visionMap.cols)
      for {
        row <- 0 until visionMap.rows
        col <- 0 until visionMap.cols
      } {
        map(row, col) = visionMap(row, col)
      }

      Observation(map, RobotPosition(Point(visionMap.rows - from.point.x, from.point.y), Direction.South))
    }

    def orientated: Observation = from.direction match {
      case Direction.North => turnNorth
      case Direction.South => turnSouth
      case Direction.West => turnWest
      case Direction.East => turnEast
    }
  }

  type Pattern = (Observation) => Double
}
