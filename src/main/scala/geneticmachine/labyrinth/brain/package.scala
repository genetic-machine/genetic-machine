package geneticmachine.labyrinth

package object brain {
  trait LabyrinthSense extends (LabyrinthInput => Seq[Double]) {
    val senseNames: Seq[String]

    override def toString: String = "Incomplete Labyrinth Heuristic: " + senseNames.mkString(", ")
  }

  case class InnerObservation(from: RobotPosition, vision: Labyrinth, senses: Array[Double])

  object InnerObservation {
    def fromLabyrinthInput(senses: List[LabyrinthSense])(input: LabyrinthInput): InnerObservation = {
      val sensesValues = senses.flatMap { sense => sense(input) }.toArray
      val obs = input.observation

      InnerObservation(obs.from, obs.visionMap, sensesValues)
    }
  }
}
