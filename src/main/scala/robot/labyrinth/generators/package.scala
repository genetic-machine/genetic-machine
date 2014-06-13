package robot.labyrinth

package object generators {
  trait LabyrinthGenerator {
    def apply(): Labyrinth
  }
}
