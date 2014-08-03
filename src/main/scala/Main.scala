import geneticmachine.machine._

object Main {
  def main(args: Array[String]) {
    new GeneticMachine(systemName = "GeneticMachine") with Neo4jDB with RemoteView with RemoteControl
  }
}