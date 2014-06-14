package robot.labyrinth.vision

import robot.labyrinth._

object DijkstraVision {
  def apply(deep: Int) = new DijkstraVision(deep)
}

class DijkstraVision(val deep: Int) extends Vision {
  def apply(labyrinth: Labyrinth, from: Point): Observation = {
    val vision = Labyrinth.unknown(2 * deep + 1, 2 * deep + 1)
    val offset = Point(deep, deep)
    vision(offset.x, offset.y) = CellStatus.Free

    def breadthFirstSearch(openSet: Set[Point], closedSet: Set[Point]) {
      val waveFront = for {
        p <- openSet
        neigh <- p.neighbors.filter { _.inBorders(labyrinth.rows, labyrinth.cols) }
        if !closedSet.contains(neigh)
        if (neigh - from).l1Norm <= deep
      } yield {
        val visP = neigh - from + offset
        vision(visP.x, visP.y) = labyrinth(neigh.x, neigh.y)
        neigh
      }

      val (openFree, closedOccupied) = waveFront.span { p =>
        labyrinth(p.x, p.y) != CellStatus.Occupied
      }

      if (openFree.nonEmpty) {
        println(printLab(vision))
        breadthFirstSearch(openFree, closedSet ++ closedOccupied ++ openSet)
      }
    }

    breadthFirstSearch(Set(from), Set.empty)

    Observation(vision, from)
  }
}
