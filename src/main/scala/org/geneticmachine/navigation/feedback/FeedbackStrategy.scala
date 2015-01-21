package org.geneticmachine.navigation.feedback

import org.geneticmachine.navigation.NavigationCommand._
import org.geneticmachine.navigation.{ NavigationCommand, NavigationState }

object FeedbackStrategyGenerator {
  def apply(gen: NavigationState => FeedbackStrategy): FeedbackStrategyGenerator = {
    new FeedbackStrategyGenerator {
      override def apply(initialState: NavigationState): FeedbackStrategy = gen(initialState)
    }
  }
}

trait FeedbackStrategyGenerator extends Serializable {

  override def toString: String = "Incomplete FeedbackStrategyGenerator"

  def apply(initialState: NavigationState): FeedbackStrategy

  def *(scale: Double) = FeedbackStrategyGenerator { initialState =>
    apply(initialState) * scale
  }

  final def &(other: FeedbackStrategyGenerator): FeedbackStrategyGenerator = {
    val self = this

    new FeedbackStrategyGenerator {
      def apply(initialState: NavigationState): FeedbackStrategy = {
        self(initialState) & other(initialState)
      }
    }
  }
}

object FeedbackStrategy {
  def apply(feedback: (NavigationState, NavigationCommand.NavigationCommand) => Double): FeedbackStrategy = {
    new FeedbackStrategy {
      override def apply(state: NavigationState, action: NavigationCommand): Double = feedback(state, action)
    }
  }
}

trait FeedbackStrategy extends FeedbackStrategyGenerator with Serializable { self =>
  override def toString: String = "Incomplete FeedbackStrategy"

  def apply(state: NavigationState, action: NavigationCommand.NavigationCommand): Double

  final def apply(initialState: NavigationState): FeedbackStrategy = self

  final override def *(scale: Double): FeedbackStrategy = FeedbackStrategy { (ns, nc) =>
    self(ns, nc) * scale
  }

  final def &(other: FeedbackStrategy) = {
    new FeedbackStrategy {
      final override def toString: String = s"$self & $other"

      final override def apply(status: NavigationState, action: NavigationCommand): Double = {
        self(status, action) + other(status, action)
      }
    }
  }
}
