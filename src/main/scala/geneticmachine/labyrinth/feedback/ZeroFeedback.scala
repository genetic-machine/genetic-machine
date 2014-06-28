package geneticmachine.labyrinth.feedback

import geneticmachine.labyrinth._

object ZeroFeedback extends FeedbackStrategy {
  def apply(status: LabyrinthStatus, action: LabyrinthCommand.LabyrinthCommand): LabyrinthScore = {
    LabyrinthScore(0.0)
  }

  override def toString(): String = "Zero feedback"
}
