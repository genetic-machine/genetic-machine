package geneticmachine.labyrinth.brain

import breeze.linalg.{ Vector => BreezeVector, _ }
import common.dataflow.DataFlowFormat._
import common.dataflow._
import geneticmachine.BrainFactory
import geneticmachine.genetic.Evolution
import geneticmachine.labyrinth._
import geneticmachine.labyrinth.heuristics._

import scala.collection.parallel.immutable.{ParRange, ParVector}
import scala.concurrent.Future
import akka.actor._

final case class FusionBrainFactory(senses: List[LabyrinthSense])
                                   (evolution: Evolution[ParVector[Gene]])
  extends BrainFactory[LabyrinthInput, LabyrinthOutput, LabyrinthFeedback] {

  def props(dff: DataFlowFormat) = Props { new FusionBrain(senses)(evolution)(dff) }

  override def toString: String = s"Fusion Brain:\n  -evolution: $evolution;\n  -senses: $senses"

  override def empty: DataFlowFormat = {
    val builder = DataFlowFormatBuilder(brainLabel)
    builder.node("LabyrinthInput").asInput()
    builder.node("LabyrinthOutput").asOutput()

    builder.toDataFlowFormat
  }
}

class FusionBrain(val senses: List[LabyrinthSense])
                 (val evolution: Evolution[ParVector[Gene]])
                 (dff: DataFlowFormat)
  extends LabyrinthBrain[Population](dff) {

  import context.dispatcher

  /**
   * Note: initialization and serialization are not parallel operations.
   */
  override protected def init: Future[Population] = Future {
    val startNode = dff.node(dff.inputNodeId)

    val entities: ParVector[Gene] = (for {
      outPort <- startNode.edges
      inPort <- outPort
      node = dff.node(inPort.nodeId)
      props = node.props
      entity <- Gene.fromProps(props)
    } yield entity).toVector.par

    Population(evolution(entities), 0)
  }

  override protected def serialize(state: Population): Future[DataFlowFormat] = Future {
    val dffBuilder = new DataFlowFormatBuilder(DataFlowFormat.brainLabel)
    val fictiveInput = dffBuilder.node("Population")("size" -> state.entities.size).asInput()
    val fictiveOutput = dffBuilder.node("Out").asOutput()

    for (entity <- state.entities.toVector) {
      val node = dffBuilder.node("pattern")(entity.asProps)
      fictiveInput --> node
      node --> fictiveOutput
    }

    dffBuilder.toDataFlowFormat
  }

  /**
   * Fusion brain hasn't any internal state so [[reset]] simply returns current state.
   */
  override protected def reset(state: Population): Future[Population] = Future.successful {
    state
  }

  private def action(genes: ParVector[Gene],
                     observation: vision.VisionObservation,
                     senses: Array[Double]): (Population, LabyrinthOutput) = {
    val gs = genes.map { gene =>
      val gain = gene.pattern.compare(observation, heuristic)
      gene.copy(currentGain = gain)
    }

    val bestGene = (0 until gs.size).par.max {
      Ordering by { i: Int =>
        gs(i).currentGain * gs(i).strength
      }
    }

    if (gs(bestGene).currentGain > 0.0) {
      (Population(gs, bestGene), gs(bestGene).action)
    } else {
      log.info(s"Forced mutation! Best gain ${gs(bestGene).currentGain};")
      val mutated = evolution(gs)
      action(mutated, observation, heuristic)
    }
  }

  override protected def input(state: Population,
                               input: LabyrinthInput): Future[(Population, LabyrinthOutput)] = Future {
    val observation = input.observation
    val heuristicResult = DijkstraSense(input)

    action(state.entities, observation, heuristicResult)
  }

  override protected def feedback(state: Population, feedback: LabyrinthFeedback): Future[Population] = Future {

    val activeG = state.entities(state.activeGene)
    val updatedG = activeG.copy(strength = activeG.strength + activeG.currentGain * feedback.value)
    val gs = state.entities.updated(state.activeGene, updatedG)

    val updatedPopulation = evolution(gs)

    Population(updatedPopulation, state.activeGene)
  }
}
