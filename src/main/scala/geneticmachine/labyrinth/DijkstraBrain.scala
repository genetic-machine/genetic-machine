package geneticmachine.labyrinth

import akka.actor.Props
import geneticmachine.{BrainFactory, Brain}
import LabyrinthCommand.LabyrinthCommand
import geneticmachine.dataflow.{DataFlowFormatBuilder, DataFlowFormat}
import geneticmachine.dataflow.DataFlowFormat._

import scala.concurrent.Future
import scala.util.Try

object DijkstraBrain extends BrainFactory[LabyrinthInput, LabyrinthOutput, LabyrinthFeedback] {

  def props(dff: DataFlowFormat) = Props(classOf[DijkstraBrain], dff)

  override def toString: String = "Dijkstra Brain"

  def empty: DataFlowFormat = {
    val builder = DataFlowFormatBuilder(brainLabel)
    val inputNode = builder.node("LabyrinthInput").asInput()
    val outputNode = builder.node("LabyrinthOutput").asOutput()
    val aiNode = builder.node("Strict AI")("method" -> "Dijkstra algorithm mod. 2")

    inputNode --> aiNode
    aiNode --> outputNode

    builder.toDataFlowFormat
  }
}

class DijkstraBrain(dff: DataFlowFormat)
  extends Brain[LabyrinthInput, LabyrinthCommand, LabyrinthFeedback, Integer](dff) {

  import context.dispatcher

  override def serialize(state: Integer) = Future.successful {
    DijkstraBrain.empty
  }

  override def init(dff: DataFlowFormat): Integer = 0

  override def input(stepCounter: Integer, data: LabyrinthInput): Future[(Integer, LabyrinthCommand)] = Future {
    val (command, _) = strictMinPathSensor(data).max(Ordering by { x: (LabyrinthCommand.LabyrinthCommand, Double) => x._2 })
    val newState: Integer = stepCounter + 1
    (newState, command)
  }

  override def feedback(state: Integer, score: LabyrinthFeedback) = Future.successful(state)

  override def reset(state: Integer) = Future.successful(state)
}
