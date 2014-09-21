package geneticmachine.labyrinth.feedback

import geneticmachine.labyrinth._

object ZeroFeedbackGenerator

object ZeroFeedback extends FeedbackStrategy {

  def apply(status: LabyrinthState, action: LabyrinthCommand.LabyrinthCommand): Double = {
    0.0
  }

  override def toString: String = "Zero feedback"
}
