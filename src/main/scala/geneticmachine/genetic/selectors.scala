package geneticmachine.genetic

import scala.collection.parallel.immutable.ParVector

object selectors {
  final case class ThresholdSelector[G <: Strength](threshold: Double) extends Selector[ParVector[G]] {
    def select(population: ParVector[G]): ParVector[G] = {
      population.filter { g: G =>
        g.strength > threshold
      }
    }
  }

  /**
   * Not parallel selector!
   */
  abstract class BestSelector[G <: Strength] extends Selector[ParVector[G]] {
    def select(n: Int)(population: ParVector[G]): ParVector[G] = {
      population.toVector.sortBy { - _.strength }.take(n).par
    }
  }


  final case class NBestSelector[G <: Strength](n: Int) extends BestSelector[G] {
    def select(population: ParVector[G]) = select(n)(population)
  }


  final case class QuantileSelector[G <: Strength](alpha: Double) extends BestSelector[G] {
    def select(population: ParVector[G]) = select((population.size * alpha).toInt)(population)
  }
}
