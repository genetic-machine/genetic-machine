import geneticmachine._
import geneticmachine.labyrinth.{LabyrinthRobot, DijkstraBrain}

object Main extends App {
  override def main(args: Array[String]) {
    using(DijkstraBrain).startWith(28).testWith(LabyrinthRobot.sampleFactory).repeat(2).execute()
  }
}