import geneticmachine._
import geneticmachine.labyrinth.{LabyrinthRobot, DijkstraBrain, LabyrinthStatus}
import geneticmachine.machine.{RemoteView, RemoteControl, Neo4jDB, GeneticMachine}
import scala.util.Success

object Main extends App {
  override def main(args: Array[String]) {
    // create local server
    new GeneticMachine(systemName = "GeneticMachine") with Neo4jDB with RemoteView with RemoteControl

    // connect to server by tcp
    val remoteMachine = remote("GeneticMachine@127.0.0.1:7778")

    import remoteMachine.dispatcher

    remoteMachine {
      using(DijkstraBrain).startWithNew.
        testWith(LabyrinthRobot.sampleFactory).repeat(3)
    }.onComplete {
      case Success(results: Experiment.Result[LabyrinthStatus]) =>
        println(results.mkString("\n"))
        sys.exit()

      case other =>
        println(other)
        sys.exit()
    }
  }
}