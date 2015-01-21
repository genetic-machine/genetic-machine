package org.geneticmachine.navigation.heuristics

import org.geneticmachine.navigation._
import org.geneticmachine.navigation.algorithm._

/**
 * Normalized distance to goal.
 */
object DistanceToGoalSense extends LabyrinthSense {

  override val senseNames: Seq[String] = Seq(name)

  def apply(input: NavigationInput): Seq[Double] = {
    val currentPosition = input.robotPosition.point
    val goal = input.goal
    val distance = (currentPosition - goal).l2Norm
    val maxDistance = Point(input.lab.cols, input.lab.rows).l2Norm

    Seq(distance / maxDistance)
  }
}
