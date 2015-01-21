package org.geneticmachine.genetic

trait GeneStrength {
  val strength: Double
}

trait Selector[P]
  extends AdaptiveSelector[P, Any] with Serializable {

  def apply(population: P): P

  def apply(input: Any): Selector[P] = this

  final def &(other: Selector[P]) = {
    val self = this

    new Selector[P] {
      final def apply(population: P): P = self(other(population))
    }
  }
}

trait AdaptiveSelector[P, -I] extends Serializable {
  def apply(input: I): Selector[P]
}