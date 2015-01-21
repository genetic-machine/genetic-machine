package org.geneticmachine.navigation.feedback

import org.geneticmachine.navigation._

object ZeroFeedbackGenerator

object ZeroFeedback extends FeedbackStrategy {

  override def toString: String = "Zero feedback"

  def apply(status: NavigationState, action: NavigationCommand): Double = 0.0
}
