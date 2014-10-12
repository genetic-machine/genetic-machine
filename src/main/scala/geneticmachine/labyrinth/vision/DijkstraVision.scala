package geneticmachine.labyrinth.vision

import geneticmachine.labyrinth._

object DijkstraVision {
  def apply(depth: Int) = new DijkstraVision(depth)
}

class DijkstraVision(val depth: Int) extends Vision {
  def apply(labyrinth: Labyrinth, from: RobotPosition): Observation = {
    val vision = Labyrinth.unknown(2 * depth + 1, 2 * depth + 1)
    val offset = Point(depth, depth)
    vision(offset.x, offset.y) = CellStatus.Free

    def breadthFirstSearch(openSet: Set[Point], closedSet: Set[Point]) {
      val waveFront = for {
        p <- openSet
        neigh <- p.neighbors.filter { _.inBorders(labyrinth.rows, labyrinth.cols) }
        if !closedSet.contains(neigh)
        if (neigh - from.point).l1Norm <= depth
      } yield {
        val visP = neigh - from.point + offset
        vision(visP.x, visP.y) = labyrinth(neigh.x, neigh.y)
        neigh
      }

      val (openFree, closedOccupied) = waveFront.span { p =>
        labyrinth(p.x, p.y) != CellStatus.Occupied
      }

      if (openFree.nonEmpty) {
        println(labToCharMatrix(vision))
        breadthFirstSearch(openFree, closedSet ++ closedOccupied ++ openSet)
      }
    }

    breadthFirstSearch(Set(from.point), Set.empty)

    Observation(vision, from)
  }
}
