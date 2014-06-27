package geneticmachine.labyrinth

import geneticmachine.Brain
import LabyrinthCommand.LabyrinthCommand
import geneticmachine.dataflow.DataFlowFormat

import scala.concurrent.Future

object DijkstraBrain {
  def serialization: DataFlowFormat = {
    DataFlowFormat.empty("DijkstraBrain", "LabyrinthInput", "LabyrinthOutput")
  }
}

class DijkstraBrain
  extends Brain[LabyrinthInput, LabyrinthCommand, LabyrinthScore, Integer](DijkstraBrain.serialization) {

  import context.dispatcher

  override def serialize(state: Integer) = Future.successful(DijkstraBrain.serialization)

  override def init(dff: DataFlowFormat): Integer = 0

  override def input(stepCounter: Integer, data: LabyrinthInput): Future[(Integer, LabyrinthCommand)] = Future {
    val (command, _) = strictMinPathSensor(data).max(Ordering by { x: (LabyrinthCommand.LabyrinthCommand, Double) => x._2 })
    val newState: Integer = stepCounter + 1
    (newState, command)
  }

  override def feedback(state: Integer, score: LabyrinthScore) = Future.successful(state)

  override def reset(state: Integer) = Future.successful(state)
}
