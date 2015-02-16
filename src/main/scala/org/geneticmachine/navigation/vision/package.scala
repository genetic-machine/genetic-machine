package org.geneticmachine.navigation

package object vision {

  type VisionMap = Labyrinth

  trait Vision extends Serializable {
    def apply(labyrinth: Labyrinth, from: RobotPosition): VisionObservation
  }

  object Vision {
  }

  case class VisionObservation(visionMap: VisionMap, from: RobotPosition) {
    private type CoordTransform = (Int, Int) => (Int, Int)

    private def counterClockwise: CoordTransform = (x, y) => (visionMap.cols - y - 1, x)
    private def clockwise: CoordTransform = (x, y) => (y, visionMap.rows - x - 1)
    private def upsideDown: CoordTransform = (x, y) => (visionMap.rows - x - 1, visionMap.cols - y - 1)

    override def toString: String = {
      val vis = labToCharMatrix(visionMap)
      vis(from.point.x, from.point.y) = Direction.char(from.direction)
      matrixToString(vis)
    }

    def impose(lab: Labyrinth): Labyrinth = {
      val r: Int = { (visionMap.rows - 1) / 2 }
      def rows = -r to r
      def cols = -r to r

      val p = from.point

      for {
        x <- rows
        y <- cols
        labX = p.x + x
        labY = p.y + y
        if labX >= 0 && labY >= 0 && labX < lab.rows && labY < lab.cols
        if visionMap(x + r, y + r) != CellStatus.Unknown
      } {
        lab(labX, labY) = visionMap(x + r, y + r)
      }

      lab
    }

    private def coordTransform(tr: CoordTransform)(m1: Labyrinth, m2: Labyrinth): Labyrinth = {
      for {
        i <- 0 until m1.rows
        j <- 0 until m1.cols
        (x, y) = tr(i, j)
      } {
        m2(x, y) = m1(i, j)
      }

      m2
    }


    private def turnNorth: VisionObservation = this

    private def turnSouth: VisionObservation = {
      val m = Labyrinth.unknown(visionMap.rows, visionMap.cols)
      coordTransform(upsideDown)(visionMap, m)
      val (fx, fy) = upsideDown(from.point.x, from.point.y)

      VisionObservation(m, RobotPosition(Point(fx, fy), Direction.North))
    }

    private def turnWest: VisionObservation = {
      val m = Labyrinth.unknown(visionMap.cols, visionMap.rows)
      coordTransform(clockwise)(visionMap, m)
      val (fx, fy) = clockwise(from.point.x, from.point.y)

      VisionObservation(m, RobotPosition(Point(fx, fy), Direction.North))
    }

    private def turnEast: VisionObservation = {
      val m = Labyrinth.unknown(visionMap.cols, visionMap.rows)
      coordTransform(counterClockwise)(visionMap, m)
      val (fx, fy) = counterClockwise(from.point.x, from.point.y)

      VisionObservation(m, RobotPosition(Point(fx, fy), Direction.North))
    }

    def orientated: VisionObservation = from.direction match {
      case Direction.North => turnNorth
      case Direction.South => turnSouth
      case Direction.West => turnWest
      case Direction.East => turnEast
    }


  }

  type Pattern = VisionObservation => Double
}
