//package org.geneticmachine.navigation.algorithm
//
//import breeze.linalg.{ Vector => BreezeVector, _ }
//import org.geneticmachine.navigation
//import org.geneticmachine.genetic.AdaptiveEvolution
//import org.geneticmachine.navigation._
//import org.geneticmachine.machine.BrainFactory
//
//import scala.collection.parallel.immutable.ParVector
//import scala.concurrent.Future
//import akka.actor._
//
//final case class FusionBrainFactory(senses: LabyrinthSense*)
//                                   (evolution: AdaptiveEvolution[ParVector[Gene], PostObservation], strengthCoef: Double = 1.0)
//  extends BrainFactory[NavigationInput, NavigationOutput, LabyrinthFeedback] {
//
//  def props(dff: DataFlowFormat) = Props { new FusionBrain(senses.toList)(evolution, strengthCoef)(dff) }
//
//  override def toString: String = s"Fusion Brain:\n  -evolution: $evolution;\n  -senses: $senses"
//
//  override def empty: DataFlowFormat = {
//    val builder = DataFlowFormatBuilder(brainLabel)
//    builder.node("LabyrinthInput").asInput()
//    builder.node("LabyrinthOutput").asOutput()
//
//    builder.toDataFlowFormat
//  }
//}
//
//class FusionBrain(val senses: List[LabyrinthSense])
//                 (val evolution: AdaptiveEvolution[ParVector[Gene], PostObservation], strengthCoef: Double)
//                 (dff: DataFlowFormat)
//  extends LabyrinthBrain[Population](dff) {
//
//  import context.dispatcher
//
//  val perception: (NavigationInput) => InnerObservation = InnerObservation.fromLabyrinthInput(senses)
//
//  /**
//   * Note: initialization and serialization are not parallel operations.
//   */
//  override protected def init: Future[Population] = Future {
//    val startNode = dff.node(dff.inputNodeId)
//
//    val entities: ParVector[Gene] = (for {
//      outPort <- startNode.edges
//      inPort <- outPort
//      node = dff.node(inPort.nodeId)
//      props = node.props
//      entity <- Gene.fromProps(props)
//    } yield entity).toVector.par
//
//    Population(entities, 0)
//  }
//
//  override protected def serialize(state: Population): Future[DataFlowFormat] = Future {
//    val dffBuilder = new DataFlowFormatBuilder(DataFlowFormat.brainLabel)
//    val fictiveInput = dffBuilder.node("Population")("size" -> state.entities.size).asInput()
//    val fictiveOutput = dffBuilder.node("Out").asOutput()
//
//    for (entity <- state.entities.toVector) {
//      val node = dffBuilder.node("pattern")(entity.asProps)
//      fictiveInput --> node
//      node --> fictiveOutput
//    }
//
//    dffBuilder.toDataFlowFormat
//  }
//
//  /**
//   * Fusion brain hasn't any internal state so [[reset]] simply returns current state.
//   */
//  override protected def reset(state: Population): Future[Population] = Future.successful {
//    state
//  }
//
//  private def forcedMutation(population: ParVector[Gene], input: NavigationInput): ParVector[Gene] = {
//    val fakePostObservation = PostObservation(perception(input), LabyrinthMutator.actionDist.draw(), 0.0)
//
//    evolution(fakePostObservation)(population)
//  }
//
//  private def action(state: Population, input: NavigationInput): (Population, NavigationOutput) = {
//    val obs = perception(input)
//
//    val genes = if (state.entities.size > 0) {
//      state.entities
//    } else {
//      log.info(s"Forced generation: size of population = 0!")
//      forcedMutation(state.entities, input)
//    }
//
//    val gs = genes.map { gene =>
//      gene.withGain {
//        gene.pattern.compare(obs)
//      }
//    }
//
//    val logistic = labyrinth.utils.logistic(strengthCoef) _
//
//    val bestGene = (0 until gs.size).par.max {
//      Ordering by { i: Int =>
//        gs(i).currentGain * logistic(gs(i).strength)
//      }
//    }
//
//    if (gs(bestGene).currentGain < 0.0) {
//      log.info(s"Forced mutation due to negative best gain = ${gs(bestGene).currentGain}!")
//      val mutated = forcedMutation(gs, input)
//      action(state.copy(entities = mutated), input)
//    } else {
//
//      val command = gs(bestGene).action
//
//      log.info {
//        s"Best gain: ${gs(bestGene).currentGain}\n" +
//          s"Best gain * str: ${gs(bestGene).currentGain * logistic(gs(bestGene).strength)}"
//      }
//
//      (Population(gs, bestGene, Some(obs), Some(command)), command)
//    }
//  }
//
//  override protected def input(state: Population,
//                               input: NavigationInput): Future[(Population, NavigationOutput)] = Future {
//    action(state, input)
//  }
//
//  override protected def feedback(state: Population, feedback: LabyrinthFeedback): Future[Population] = Future {
//    log.info(s"Feedback: ${feedback.value}")
//    val command = state.command.get
//
//    val bestGene = state.entities(state.activeGene)
//
//    log.info(s"Best gene:\n$bestGene")
//
//    log.info {
//      s"Delta str: ${bestGene.withFeedback(command)(feedback.value).strength - bestGene.strength}" +
//      s"/ ${bestGene.strength}"
//    }
//
//    val populationWithFeedback = state.entities.map {
//      g => g.withFeedback(command)(feedback.value)
//    }
//
//    val postObservation = PostObservation(state.input.get, command, feedback.value)
//    val updatedPopulation = evolution(postObservation)(populationWithFeedback)
//
//    Population(updatedPopulation, state.activeGene)
//  }
//}
