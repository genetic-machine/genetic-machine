package org.geneticmachine.navigation

import org.geneticmachine.{ Metric, ContinuousMetric }

package object metrics {

  type NavigationMetric = Metric[NavigationState]
  type ContinuousNavigationMetric = ContinuousMetric[NavigationState]

  object ManhattanDistanceToTarget extends ContinuousNavigationMetric("ManhattanDistance") {
    def apply(state: NavigationState): Seq[Double] = {
      val target = state.goal
      state.path.map { p =>
        (p.point - target).l1Norm.toDouble
      }
    }
  }

  object EuclideanDistanceToTarget extends ContinuousNavigationMetric("EuclideanDistance") {
    def apply(state: NavigationState): Seq[Double] = {
      val target = state.goal
      state.path.map { p =>
        (p.point - target).l2Norm
      }
    }
  }

  object CommandNumber extends NavigationMetric("Commands") {
    def apply(state: NavigationState): Double = {
      state.history.size.toDouble
    }
  }

  object CommandNumberToOptimal extends NavigationMetric("CommandsToOptimal") {
    def apply(state: NavigationState): Double = {
      val command = state.history.size.toDouble

      val cost = reverseCostDict(state.labyrinth, state.goal)
      val optimal = cost(state.path(0)).toDouble

      command / optimal
    }
  }
}
