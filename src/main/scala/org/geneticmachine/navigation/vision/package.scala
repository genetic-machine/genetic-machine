package org.geneticmachine.navigation

package object vision {
  trait Vision extends Serializable {
    def apply(labyrinth: Labyrinth, from: RobotPosition): VisionObservation
  }

  final case class VisionObservation(visionMap: Labyrinth, from: RobotPosition) {

    override def toString: String = s"Observation($from):\n$visionMap"

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

    def turn(coordTransform: (Int, Int) => (Int, Int), size: (Int, Int)): Labyrinth = {
      val map = Labyrinth.unknown(size._1, size._2)

      for (row <- 0 until visionMap.rows; col <- 0 until visionMap.cols) {
        val (tRow, tCol) = coordTransform(row, col)
        map(tRow, tCol) = visionMap(row, col)
      }

      map
    }

    val eastTransform = (row: Int, col: Int) => (col, visionMap.rows - row - 1)
    val westTransform = (row: Int, col: Int) => (visionMap.cols - col - 1, row)
    val southTransform = (row: Int, col: Int) => (visionMap.rows - row - 1, visionMap.cols - col - 1)

    def turnNorth = this

    def turnEast: VisionObservation = {
      val tPos = RobotPosition(from.point.map(eastTransform), from.direction.turnRight)
      VisionObservation(turn(eastTransform, (visionMap.cols, visionMap.rows)), tPos)
    }

    def turnWest: VisionObservation = {
      val tPos = RobotPosition(from.point.map(westTransform), from.direction.turnLeft)
      VisionObservation(turn(westTransform, (visionMap.cols, visionMap.rows)), tPos)
    }

    def turnSouth: VisionObservation = {
      val tPos = RobotPosition(from.point.map(southTransform), from.direction.reverse)
      VisionObservation(turn(southTransform, (visionMap.rows, visionMap.cols)), tPos)
    }

    def orientated: VisionObservation = from.direction match {
      case Direction.North => turnNorth
      case Direction.South => turnSouth
      case Direction.West => turnWest
      case Direction.East => turnEast
    }
  }

  type Pattern = (VisionObservation) => Double
}
