package geneticmachine.labyrinth.brain

import akka.actor.Props
import common.dataflow.DataFlowFormat._
import common.dataflow.{DataFlowFormat, DataFlowFormatBuilder}
import geneticmachine.labyrinth.LabyrinthCommand.LabyrinthCommand
import geneticmachine.labyrinth._
import geneticmachine.{Brain, BrainFactory}
import geneticmachine.labyrinth.heuristics.DijkstraHeuristicSensor

import scala.concurrent.Future

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

  override def init: Future[Integer] = Future.successful { 0: Integer }

  override def input(stepCounter: Integer, data: LabyrinthInput): Future[(Integer, LabyrinthCommand)] = Future {
    val command = DijkstraHeuristicSensor.optimalCommand(data)
    val newState: Integer = stepCounter + 1
    (newState, command)
  }

  override def feedback(state: Integer, score: LabyrinthFeedback) = Future.successful(state)

  override def reset(state: Integer) = Future.successful(state)
}
