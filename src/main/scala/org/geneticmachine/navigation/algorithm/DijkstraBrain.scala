package org.geneticmachine.navigation.algorithm

import akka.actor.Props
import common.dataflow.DataFlowFormat._
import common.dataflow.{DataFlowFormat, DataFlowFormatBuilder}
import org.geneticmachine.navigation.NavigationCommand.NavigationCommand
import org.geneticmachine.navigation._
import org.geneticmachine.BrainFactory
import org.geneticmachine.navigation.heuristics.DijkstraSense
import org.geneticmachine.machine.{BrainFactory, BrainActor$}

import scala.concurrent.Future

object DijkstraBrain extends BrainFactory[NavigationInput, NavigationOutput, LabyrinthFeedback] {

  def props(dff: DataFlowFormat) = Props(classOf[DijkstraBrain], dff)

  override def toString: String = "Dijkstra Brain"

  override def empty: DataFlowFormat = {
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
  extends BrainActor[NavigationInput, NavigationCommand, LabyrinthFeedback, Integer](dff) {

  import context.dispatcher

  override def serialize(state: Integer) = Future.successful {
    DijkstraBrain.empty
  }

  override def init: Future[Integer] = Future.successful { 0: Integer }

  override def input(stepCounter: Integer, data: NavigationInput): Future[(Integer, NavigationCommand)] = Future {
    val command = DijkstraSense.optimal(data)
    val newState: Integer = stepCounter + 1
    (newState, command)
  }

  override def feedback(state: Integer, score: LabyrinthFeedback) = Future.successful(state)

  override def reset(state: Integer) = Future.successful(state)
}
