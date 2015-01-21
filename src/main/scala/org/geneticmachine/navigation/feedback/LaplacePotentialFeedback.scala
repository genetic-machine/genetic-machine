package org.geneticmachine.navigation.feedback

import org.geneticmachine.navigation._

object LaplacePotentialFeedback {
  def apply(scalePos: Double, scaleNeg: Double) = FeedbackStrategyGenerator { initialState: NavigationState =>
    val costDict = reverseCostDict(initialState.labyrinth, initialState.goal)
    new LaplacePotentialFeedback(costDict, scalePos, scaleNeg)
  }

  def apply(scalePos: Double) = apply(scalePos, scalePos)
}

class LaplacePotentialFeedback(val costDict: CostDict, val scalePos: Double, val scaleNeg: Double) extends FeedbackStrategy {
  override def toString: String = "Laplace's potential feedback"

  def apply(status: NavigationState, action: NavigationCommand.NavigationCommand): Double = {
    val currentCost = costDict(status.robotPosition)
    val updatedCost = costDict(status.robotPosition.action(status.labyrinth)(action))

    val f = currentCost - updatedCost

    (if (f < 0.0) scaleNeg else scalePos) * f
  }
}


case class HistoryFeedback(feedback: Double, repetition: Int = 5) extends FeedbackStrategy {
  override def toString: String = "History feedback"

  def apply(state: NavigationState, action: NavigationCommand.NavigationCommand): Double = {
    val nextPosition = state.robotPosition.action(state.labyrinth)(action)
    val sameHist = state.path.filter { p =>
      p == nextPosition
    }

    if (sameHist.size > repetition) {
      feedback
    } else {
      0.0
    }
  }
}