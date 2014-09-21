package geneticmachine.labyrinth.heuristics

import geneticmachine.labyrinth._

/**
 * Quite good heuristic for min path search in partially observed domain.
 * It simply applies Dijkstra algorithm as search for optimal action, considering [[CellStatus.Unknown]] cells
 * as [[CellStatus.Free]] cells.
 *
 * Returns map from actions to their values where values can be interpreted as level of
 * assurance, an action leads to the optimal path. For this heuristic, it's guarantied
 * only optimal (by heuristic) commands have value `1.0`.
 * Non-optimal commands are marked with zero value.
 */
object DijkstraHeuristicSensor extends LabyrinthHeuristic {
  private def argmin[K, V : Ordering](kvs: Seq[(K, V)]): Seq[K] = {
    val min: V = kvs.min {
      Ordering by { kv: (K, V) => kv._2 }
    }._2

    kvs.filter { kv: (K, V) =>
      kv._2 == min
    }.map { kv: (K, V) =>
      kv._1
    }
  }

  private def dijkstraHeuristicSensor(lab: Labyrinth, from: RobotPosition,
                                      reverseCost: CostDict): CommandSignal = {
    val minCostPoses = argmin {
      for {
        (command, pos) <- from.actions(lab)
      } yield (command, reverseCost(pos))
    }

    CommandSignal {
      minCostPoses.map { command =>
        (command, 1.0)
      }.toMap
    }
  }

  override def apply(labInput: LabyrinthInput): CommandSignal = {
    val LabyrinthInput(lab, from, goal) = labInput
    val cost = reverseCostDict(lab, goal)
    dijkstraHeuristicSensor(lab, from, cost)
  }

  override def toString: String = "Dijkstra Heuristic"
}
