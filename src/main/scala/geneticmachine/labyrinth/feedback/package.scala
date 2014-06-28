package geneticmachine.labyrinth

package object feedback {
  trait FeedbackStrategy {
    def apply(status: LabyrinthStatus, action: LabyrinthCommand.LabyrinthCommand): LabyrinthScore
  }
}
