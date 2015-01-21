package org.geneticmachine.genetic.evolution

import breeze.stats.distributions.Rand

import org.geneticmachine.genetic._

import scala.collection.parallel.immutable.ParVector

object SafeEvolution {
  def adaptive[G, I](populationLimit: Int, crossoverLimit: Int,
                     mutationLimit: Int, generationLimit: Int) = {
    Evolution.adaptive[ParVector[G], G, I] {
      SafeEvolution[G](populationLimit, crossoverLimit, mutationLimit, generationLimit)
    } _
  }
}

/**
 * Saves selected genes.
 */
final case class SafeEvolution[T](populationLimit: Int, crossoverLimit: Int,
                                  mutationLimit: Int, generationLimit: Int)
                                 (mutator: Mutator[T], selector: Selector[ParVector[T]])
  extends Evolution[ParVector[T]] {

  override def apply(population: ParVector[T]): ParVector[T] = {
    val selected = selector(population)

    if (selected.size == 0) {
      parRange(0, populationLimit).map { _ =>
        mutator()
      }.toVector.par
    } else {
      val dist = Rand.choose(selected.seq)

      val toCrossover = (populationLimit - selected.size) min crossoverLimit

      val crossovered = for {
        _ <- parRange(0, toCrossover)
        g1 = dist.draw()
        g2 = dist.draw()
      } yield mutator(g1, g2)

      val toMutate = (populationLimit - selected.size - crossovered.size) min mutationLimit
      val mutated = for {
        _ <- parRange(0, toMutate)
        g = dist.draw()
      } yield mutator(g)

      val toGenerate = (populationLimit - selected.size - crossovered.size - mutated.size) min generationLimit

      val generated = for {
        _ <- parRange(0, toGenerate)
      } yield mutator()

      selected ++ crossovered ++ mutated ++ generated
    }
  }

}