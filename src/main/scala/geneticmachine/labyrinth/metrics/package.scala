package geneticmachine.labyrinth

import geneticmachine.{ Metric, ContinuousMetric }

package object metrics {

  type LabyrinthMetric = Metric[LabyrinthStatus]
  type ContinuousLabyrinthMetric = ContinuousMetric[LabyrinthStatus]

  object ManhattanDistanceToTarget extends ContinuousLabyrinthMetric("ManhattanDistance") {
    def apply(state: LabyrinthStatus): Seq[Double] = {
      val target = state.goal
      state.path.map { p =>
        (p - target).l1Norm.toDouble
      }
    }
  }

  object EuclideanDistanceToTarget extends ContinuousLabyrinthMetric("EuclideanDistance") {
    def apply(state: LabyrinthStatus): Seq[Double] = {
      val target = state.goal
      state.path.map { p =>
        (p - target).l2Norm
      }
    }
  }

  object CommandNumber extends LabyrinthMetric("Commands") {
    def apply(state: LabyrinthStatus): Double = {
      state.history.size.toDouble
    }
  }
}
