package geneticmachine.labyrinth.feedback

import geneticmachine.labyrinth.LabyrinthCommand._
import geneticmachine.labyrinth.{ LabyrinthCommand, LabyrinthState }

trait FeedbackStrategyGenerator[+FS <: FeedbackStrategy] extends Serializable {

  override def toString: String = "Incomplete FeedbackStrategyGenerator"

  def apply(initialState: LabyrinthState): FS

  final def &(other: FeedbackStrategyGenerator[FeedbackStrategy]): FeedbackStrategyGenerator[FeedbackStrategy] = {
    val self = this

    new FeedbackStrategyGenerator[FeedbackStrategy] {
      def apply(initialState: LabyrinthState): FeedbackStrategy = {
        self(initialState) & other(initialState)
      }
    }
  }
}

trait FeedbackStrategy extends FeedbackStrategyGenerator[FeedbackStrategy] with Serializable {
  override def toString: String = "Incomplete FeedbackStrategy"

  def apply(state: LabyrinthState, action: LabyrinthCommand.LabyrinthCommand): Double

  final def apply(initialState: LabyrinthState): this.type = this

  final def &(other: FeedbackStrategy) = {
    val self: FeedbackStrategy = this

    new FeedbackStrategy {
      final override def toString: String = s"$self & $other"

      final override def apply(status: LabyrinthState, action: LabyrinthCommand): Double = {
        self(status, action) + other(status, action)
      }
    }
  }
}
