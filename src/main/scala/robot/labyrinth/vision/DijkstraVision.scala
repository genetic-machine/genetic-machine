package robot.labyrinth.vision

import robot.labyrinth._

class DijkstraVision(val deep: Int) extends Vision {
  def apply(labyrinth: Labyrinth, from: Point): Observation = {
    val vision = Labyrinth.unknown(2 * deep + 1, 2 * deep + 1)
    val offset = Point(deep, deep)

    def deepFirstSearch(openSet: Set[Point], closedSet: Set[Point]) {
      val waveFront = for {
        p <- openSet
        neigh <- p.neighborsInLabyrinth(labyrinth)
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
        deepFirstSearch(openFree, closedSet ++ closedOccupied)
      }
    }

    deepFirstSearch(Set(from), Set.empty)

    Observation(vision, from)
  }
}
