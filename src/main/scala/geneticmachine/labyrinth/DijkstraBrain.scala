package geneticmachine.labyrinth

import geneticmachine.Brain
import LabyrinthCommand.LabyrinthCommand
import geneticmachine.ubf.UnifiedBrainFormat

import scala.concurrent.Future

object DijkstraBrain {
  def serialization: UnifiedBrainFormat = {
    UnifiedBrainFormat.empty("DijkstraBrain", "LabyrinthInput", "LabyrinthOutput", -1)
  }
}

class DijkstraBrain
  extends Brain[LabyrinthInput, LabyrinthCommand, LabyrinthScore, Integer](DijkstraBrain.serialization) {

  import context.dispatcher

  override def serialize(state: Integer) = Future.successful(DijkstraBrain.serialization)

  override def init() = Future.successful(0)

  override def process(stepCounter: Integer, data: LabyrinthInput): Future[(Integer, LabyrinthCommand)] = Future {
    val (command, _) = strictMinPathSensor(data).max(Ordering by { x: (LabyrinthCommand.LabyrinthCommand, Double) => x._2 })
    val newState: Integer = stepCounter + 1
    (newState, command)
  }

  override def feedback(state: Integer, score: LabyrinthScore) = Future.successful(state)

  override def reset(state: Integer) = Future.successful(state)
}
