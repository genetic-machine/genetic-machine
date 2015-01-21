package org.geneticmachine.navigation.feedback

import org.geneticmachine.navigation._

/**
 * Returns [[feedback]] if `action` is impossible otherwise zero.
 * @param feedback should be negative.
 */
case class ImpossibleActionFeedback(feedback: Double) extends FeedbackStrategy {

  override def toString: String = "Impossible action feedback"

  final def apply(status: NavigationState, action: NavigationCommand.NavigationCommand): Double = {
    if (status.robotPosition(action).inLabyrinth(status.labyrinth)) {
      0.0
    } else {
      feedback
    }
  }
}
