package org.geneticmachine.navigation.heuristics

import org.geneticmachine.navigation._
import org.geneticmachine.navigation.algorithm._

object GoalDirectionSense extends LabyrinthSense {

  override val senseNames: Seq[String] = Seq("onTheRight", "straight", "onTheRight")

  override def apply(input: NavigationInput): Seq[Double] = {
    val p = input.robotPosition.point
    val dir = input.robotPosition.direction
    val goal = input.goal

    val forward = (goal - p).angle(dir)
    val left = (goal - p).angle(dir.turnLeft)
    val rigth = (goal - p).angle(dir.turnRight)

    Seq(left, forward, rigth)
  }
}
