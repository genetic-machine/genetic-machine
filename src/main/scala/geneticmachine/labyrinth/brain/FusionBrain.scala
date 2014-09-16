package geneticmachine.labyrinth.brain

import breeze.linalg.{ Vector => BreezeVector, _ }
import common.dataflow._
import geneticmachine.genetic.Evolution
import geneticmachine.labyrinth._

import scala.collection.parallel.immutable.ParVector
import scala.concurrent.Future
import akka.actor._

class FusionBrain(val evolution: Evolution[ParVector[Gene]])
                  (dff: DataFlowFormat) extends LabyrinthBrain[Population](dff) {

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

    val lastAction = LabyrinthCommand(0)

    Population(evolution(entities), lastAction)
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

  override protected def reset(state: Population): Future[Population] = Future.successful {
    state
  }

  override protected def input(state: Population,
                               input: LabyrinthInput): Future[(Population, LabyrinthOutput)] = Future {
    val observation = input.observation
    val (heuristic, _) = strictMinPathSensor(input).max {
      Ordering by { x: (LabyrinthCommand.LabyrinthCommand, Double) => x._2}
    }

    val gs = state.entities.map { entity =>
      val gain = entity.pattern.compare(observation, heuristic)
      entity.copy(currentGain = gain)
    }

    val best = gs.max {
      Ordering by { entity: Gene => entity.currentGain * entity.strength }
    }

    (Population(gs, best.action), best.action)
  }

  override protected def feedback(state: Population, feedback: LabyrinthFeedback): Future[Population] = Future {
    val gs = state.entities.map { g: Gene =>
      val strength = if (g.action == state.lastAction) {
        g.strength * (1.0 + feedback.value * g.currentGain)
      } else {
        g.strength
      }

      g.copy(strength = strength)
    }

    Population(evolution(gs), state.lastAction)
  }
}
