package geneticmachine.labyrinth.feedback

import geneticmachine.labyrinth._

object ZeroFeedback extends FeedbackStrategy {
  def apply(status: LabyrinthState, action: LabyrinthCommand.LabyrinthCommand): LabyrinthFeedback = {
    LabyrinthFeedback(0.0)
  }

  override def toString(): String = "Zero feedback"
}
