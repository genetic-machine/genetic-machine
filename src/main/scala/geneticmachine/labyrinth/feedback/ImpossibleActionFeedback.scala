package geneticmachine.labyrinth.feedback

import geneticmachine.labyrinth._

/**
 * Returns [[feedback]] if `action` is impossible otherwise zero.
 * @param feedback should be negative.
 */
case class ImpossibleActionFeedback(feedback: Double) extends FeedbackStrategy {

  override def toString: String = "Impossible action feedback"

  final def apply(status: LabyrinthState, action: LabyrinthCommand.LabyrinthCommand): Double = {
    if (status.robotPosition(action).inLabyrinth(status.labyrinth)) {
      0.0
    } else {
      feedback
    }
  }
}
