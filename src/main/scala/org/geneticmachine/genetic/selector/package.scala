package org.geneticmachine.genetic

import scala.collection.parallel.immutable.ParVector

package object selector {
  final case class ThresholdSelector[G <: GeneStrength](threshold: Double) extends Selector[ParVector[G]] {
    def apply(population: ParVector[G]): ParVector[G] = {
      population.filter { g: G =>
        g.strength > threshold
      }
    }
  }

  /**
   * Not a parallel selector!
   */
  abstract class BestSelector[G <: GeneStrength] extends Selector[ParVector[G]] {
    def apply(n: Int)(population: ParVector[G]): ParVector[G] = {
      population.toVector.sortBy { - _.strength }.take(n).par
    }
  }


  final case class NBestSelector[G <: GeneStrength](n: Int) extends BestSelector[G] {
    def apply(population: ParVector[G]): ParVector[G] = apply(n)(population)
  }


  final case class QuantileSelector[G <: GeneStrength](alpha: Double) extends BestSelector[G] {
    def apply(population: ParVector[G]): ParVector[G] = apply((population.size * alpha).toInt)(population)
  }
}
