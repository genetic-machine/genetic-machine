package geneticmachine.labyrinth

import geneticmachine.{ Metric, ContinuousMetric }

package object metrics {

  type LabyrinthMetric = Metric[LabyrinthState]
  type ContinuousLabyrinthMetric = ContinuousMetric[LabyrinthState]

  object ManhattanDistanceToTarget extends ContinuousLabyrinthMetric("ManhattanDistance") {
    def apply(state: LabyrinthState): Seq[Double] = {
      val target = state.goal
      state.path.map { p =>
        (p.point - target).l1Norm.toDouble
      }
    }
  }

  object EuclideanDistanceToTarget extends ContinuousLabyrinthMetric("EuclideanDistance") {
    def apply(state: LabyrinthState): Seq[Double] = {
      val target = state.goal
      state.path.map { p =>
        (p.point - target).l2Norm
      }
    }
  }

  object CommandNumber extends LabyrinthMetric("Commands") {
    def apply(state: LabyrinthState): Double = {
      state.history.size.toDouble
    }
  }

  object CommandNumberToOptimal extends LabyrinthMetric("CommandsToOptimal") {
    def apply(state: LabyrinthState): Double = {
      val command = state.history.size.toDouble

      val cost = reverseCostDict(state.labyrinth, state.goal)
      val optimal = cost(state.path(0)).toDouble

      command / optimal
    }
  }
}
