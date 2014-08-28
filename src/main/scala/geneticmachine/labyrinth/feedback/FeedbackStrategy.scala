package geneticmachine.labyrinth.feedback

import geneticmachine.labyrinth.{LabyrinthFeedback, LabyrinthCommand, LabyrinthState}

trait FeedbackStrategy extends Serializable {
  def apply(status: LabyrinthState, action: LabyrinthCommand.LabyrinthCommand): LabyrinthFeedback
}
