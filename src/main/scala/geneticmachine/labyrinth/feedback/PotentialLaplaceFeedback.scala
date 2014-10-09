package geneticmachine.labyrinth.feedback

import geneticmachine.labyrinth._

case class PotentialLaplaceFeedback(scale: Double) extends FeedbackStrategyGenerator[PotentialLaplaceFeedbackImpl] {
  def apply(initialState: LabyrinthState): PotentialLaplaceFeedbackImpl = {
    val costDict = reverseCostDict(initialState.labyrinth, initialState.goal)
    new PotentialLaplaceFeedbackImpl(costDict, scale)
  }
}

class PotentialLaplaceFeedbackImpl(val costDict: CostDict, val scale: Double) extends FeedbackStrategy {
  override def toString: String = "Potential Laplace's feedback"

  def apply(status: LabyrinthState, action: LabyrinthCommand.LabyrinthCommand): Double = {
    val currentCost = costDict(status.robotPosition)
    val updatedCost = costDict(status.robotPosition.action(status.labyrinth)(action))

    (currentCost - updatedCost) * scale
  }
}

case class AdvancedPotentialLaplaceFeedback(scalePos: Double, scaleNeg: Double) extends FeedbackStrategyGenerator[AdvancedPotentialLaplaceFeedbackImpl] {
  def apply(initialState: LabyrinthState): AdvancedPotentialLaplaceFeedbackImpl = {
    val costDict = reverseCostDict(initialState.labyrinth, initialState.goal)
    new AdvancedPotentialLaplaceFeedbackImpl(costDict, scalePos, scaleNeg)
  }
}

class AdvancedPotentialLaplaceFeedbackImpl(val costDict: CostDict, val scalePos: Double, val scaleNeg: Double) extends FeedbackStrategy {
  override def toString: String = "Potential Laplace's feedback"

  def apply(status: LabyrinthState, action: LabyrinthCommand.LabyrinthCommand): Double = {
    val currentCost = costDict(status.robotPosition)
    val updatedCost = costDict(status.robotPosition.action(status.labyrinth)(action))

    val feedback = currentCost - updatedCost
    (if (feedback > 0.0) scalePos else scaleNeg) * feedback
  }
}