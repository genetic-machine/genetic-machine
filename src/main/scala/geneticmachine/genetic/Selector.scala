package geneticmachine.genetic

import scala.collection.parallel.immutable.ParVector

/**
 * Represents selection.
 */
trait Selector[P] extends Serializable {
  final def apply(population: P): P = select(population)

  def select(population: P): P
}

trait Strength {
  def strength: Double
}

final case class ThresholdSelect[G <: Strength](threshold: Double) extends Selector[ParVector[G]] {
  def select(population: ParVector[G]): ParVector[G] = {
    population.filter { g: G =>
      g.strength > threshold
    }
  }
}