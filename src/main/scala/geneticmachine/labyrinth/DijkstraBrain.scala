package geneticmachine.labyrinth

import geneticmachine.Brain
import LabyrinthCommand.LabyrinthCommand
import geneticmachine.dataflow.{DataFlowFormatBuilder, DataFlowFormat}
import geneticmachine.dataflow.DataFlowFormat._

import scala.concurrent.Future
import scala.util.Try

object DijkstraBrain {
  def serialization(parentID: Option[Long] = None): DataFlowFormat = {
    val builder = DataFlowFormatBuilder(brainLabel)
    val inputNode = builder.node("LabyrinthInput").asInput()
    val outputNode = builder.node("LabyrinthOutput").asOutput()
    val aiNode = builder.node("Strict AI")("method" -> "Dijkstra algorithm mod. 2")

    inputNode --> aiNode
    aiNode --> outputNode

    for {
      id <- parentID
    } {
      builder(parentRelation) --> id
    }

    builder.toDataFlowFormat
  }
}

class DijkstraBrain(dff: DataFlowFormat)
  extends Brain[LabyrinthInput, LabyrinthCommand, LabyrinthScore, Integer](dff) {

  import context.dispatcher

  override def serialize(state: Integer) = Future.successful {
    val parentID = Try {
      dff.props("$id").asInstanceOf[Long]
    }.toOption

    DijkstraBrain.serialization(parentID)
  }

  override def init(dff: DataFlowFormat): Integer = 0

  override def input(stepCounter: Integer, data: LabyrinthInput): Future[(Integer, LabyrinthCommand)] = Future {
    val (command, _) = strictMinPathSensor(data).max(Ordering by { x: (LabyrinthCommand.LabyrinthCommand, Double) => x._2 })
    val newState: Integer = stepCounter + 1
    (newState, command)
  }

  override def feedback(state: Integer, score: LabyrinthScore) = Future.successful(state)

  override def reset(state: Integer) = Future.successful(state)
}
