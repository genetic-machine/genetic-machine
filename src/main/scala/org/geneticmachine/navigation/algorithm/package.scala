package org.geneticmachine.navigation

import org.geneticmachine.genetic.Mutator

package object algorithm {
  trait LabyrinthSense extends (NavigationInput => Seq[Double]) {
    val senseNames: Seq[String]
    val name: String = this.getClass.getSimpleName

    final override def toString: String = s"$name: s${senseNames.mkString(", ")}"
  }

  case class InnerObservation(from: RobotPosition, vision: Labyrinth, senses: Array[Double])

  object InnerObservation {
    def fromLabyrinthInput(senses: List[LabyrinthSense])(input: NavigationInput): InnerObservation = {
      val sensesValues = senses.flatMap { sense => sense(input) }.toArray
      val obs = input.observation

      InnerObservation(obs.from, obs.visionMap, sensesValues)
    }
  }
}
