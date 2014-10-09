package geneticmachine.labyrinth.brain

import akka.actor.Props
import common.dataflow.DataFlowFormat._
import common.dataflow.{DataFlowFormat, DataFlowFormatBuilder}
import geneticmachine.labyrinth.LabyrinthCommand.LabyrinthCommand
import geneticmachine.labyrinth._
import geneticmachine.{Brain, BrainFactory}

import breeze.stats.distributions._

import scala.concurrent.Future

object BrownianBrain extends BrainFactory[LabyrinthInput, LabyrinthOutput, LabyrinthFeedback] {

  def props(dff: DataFlowFormat) = Props(classOf[BrownianBrain], dff)

  override def toString: String = "Dijkstra Brain"

  override def empty: DataFlowFormat = {
    val builder = DataFlowFormatBuilder(brainLabel)
    val inputNode = builder.node("LabyrinthInput").asInput()
    val outputNode = builder.node("LabyrinthOutput").asOutput()
    val aiNode = builder.node("Strict AI")("method" -> "Brownian motion")

    inputNode --> aiNode
    aiNode --> outputNode

    builder.toDataFlowFormat
  }
}

class BrownianBrain(dff: DataFlowFormat)
  extends Brain[LabyrinthInput, LabyrinthCommand, LabyrinthFeedback, Integer](dff) {

  import context.dispatcher

  val commandDist = for {
    cId <- Rand.randInt(3)
  } yield LabyrinthCommand(cId)

  override def serialize(state: Integer) = Future.successful {
    DijkstraBrain.empty
  }

  override def init: Future[Integer] = Future.successful { 0: Integer }

  override def input(stepCounter: Integer, data: LabyrinthInput): Future[(Integer, LabyrinthCommand)] = Future {
    val command = commandDist.draw()
    val newState: Integer = stepCounter + 1
    (newState, command)
  }

  override def feedback(state: Integer, score: LabyrinthFeedback) = Future.successful(state)

  override def reset(state: Integer) = Future.successful(state)
}
