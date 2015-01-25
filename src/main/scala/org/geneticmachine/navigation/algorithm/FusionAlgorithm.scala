package org.geneticmachine.navigation.algorithm

import breeze.linalg.{ Vector => BreezeVector, _ }
import org.geneticmachine._
import org.geneticmachine.common.graph.{GraphBuilder, Graph}
import org.geneticmachine.genetic.AdaptiveEvolution
import org.geneticmachine.navigation._

import scala.collection.parallel.immutable.ParVector
import scala.concurrent.Future

final case class FusionAlgorithm(senses: LabyrinthSense*)
                                (evolution: AdaptiveEvolution[ParVector[Gene], PostObservation], strengthCoef: Double = 1.0)
  extends NavigationAlgorithmGen[ExecutionContext] {

  override def apply(c: ExecutionContext): NavigationAlgorithm = {
    new FusionAlgorithmImpl(c)(senses.toList)(evolution, strengthCoef)
  }

  override def toString: String = s"Fusion Brain:\n  -evolution: $evolution;\n  -senses: ${senses.mkString(", ") }"
}

class FusionAlgorithmImpl(val context: ExecutionContext)
                         (val senses: List[LabyrinthSense])
                         (val evolution: AdaptiveEvolution[ParVector[Gene], PostObservation], strengthCoef: Double)
  extends NavigationAlgorithm {

  type StateT = Population

  import context.futureExecutionContext

  val perception: (NavigationInput) => InnerObservation = InnerObservation.fromLabyrinthInput(senses)

  val log = context.logger

  /**
   * Note: initialization and serialization are not parallel operations.
   */
  override def init(parent: Option[Graph]): Future[Population] = Future {
    val parentalPopulation = for {
      graph <- parent
    } yield {
      val startNode = graph.node(graph.inputNodeId)

      val entities: ParVector[Gene] = (for {
        outPort <- startNode.edges
        inPort <- outPort
        node = graph.node(inPort.nodeId)
        props = node.props
        entity <- Gene.fromProps(props)
      } yield entity).toVector.par

      Population(entities, 0)
    }

    parentalPopulation.getOrElse(Population(ParVector.empty, 0))
  }

  override def serialize(state: Population): Future[Graph] = Future {
    val graphBuilder = new GraphBuilder(Graph.algorithmLabel)

    val input = graphBuilder.node("NavigationInput")("size" -> state.entities.size).asInput()
    val output = graphBuilder.node("NavigationOutput").asOutput()

    for (entity <- state.entities.toVector) {
      val node = graphBuilder.node("pattern")(entity.asProps)
      input --> node
      node --> output
    }

    graphBuilder.toGraph
  }

  /**
   * Fusion algorithm hasn't any internal state so [[reset]] simply returns current state.
   */
  override def reset(state: Population): Future[Population] = Future.successful {
    state
  }

  private def forcedMutation(population: ParVector[Gene], input: NavigationInput): ParVector[Gene] = {
    val fakePostObservation = PostObservation(perception(input), FusionMutator.actionDist.draw(), 0.0)

    evolution(fakePostObservation)(population)
  }

  private def action(state: Population, input: NavigationInput): (Population, NavigationOutput) = {
    val obs = perception(input)

    val genes = if (state.entities.size > 0) {
      state.entities
    } else {
      log.info {
        s"Forced generation: size of population = 0!"
      }

      forcedMutation(state.entities, input)
    }

    val gs = genes.map { gene =>
      gene.withGain {
        gene.pattern.compare(obs)
      }
    }

    val logistic = utils.logistic(strengthCoef) _

    val bestGene = (0 until gs.size).par.max {
      Ordering by { i: Int =>
        gs(i).currentGain * logistic(gs(i).strength)
      }
    }

    if (gs(bestGene).currentGain < 0.0) {
      log.info{
        s"Forced mutation due to negative best gain = ${gs(bestGene).currentGain}!"
      }

      val mutated = forcedMutation(gs, input)
      action(state.copy(entities = mutated), input)
    } else {

      val command = gs(bestGene).action

      log.info {
        s"Best gain: ${gs(bestGene).currentGain}\n" +
          s"Best gain * str: ${gs(bestGene).currentGain * logistic(gs(bestGene).strength)}"
      }

      (Population(gs, bestGene, Some(obs), Some(command)), command)
    }
  }

  override def act(state: Population, input: NavigationInput): Future[(Population, NavigationOutput)] = {
    for {
      _state <- feedback(state, input.feedback)
    } yield action(_state, input)
  }

  protected def feedback(state: Population, feedback: Double): Future[Population] = Future {
    log.info(s"Feedback: $feedback")
    val command = state.command.get

    val bestGene = state.entities(state.activeGene)

    log.info(s"Best gene:\n$bestGene")

    log.info {
      s"Delta str: ${bestGene.withFeedback(command)(feedback).strength - bestGene.strength}" +
      s"/ ${bestGene.strength}"
    }

    val populationWithFeedback = state.entities.map {
      g => g.withFeedback(command)(feedback)
    }

    val postObservation = PostObservation(state.input.get, command, feedback)
    val updatedPopulation = evolution(postObservation)(populationWithFeedback)

    Population(updatedPopulation, state.activeGene)
  }
}
