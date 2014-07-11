package geneticmachine.labyrinth

package object feedback {
  trait FeedbackStrategy extends Serializable {
    def apply(status: LabyrinthStatus, action: LabyrinthCommand.LabyrinthCommand): LabyrinthFeedback
  }
}
