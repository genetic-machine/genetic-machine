package geneticmachine.genetic

/**
 * Represents selection.
 */
trait Selector[P] extends Serializable {
  def apply(population: P): P

  final def &(other: Selector[P]) = {
    val self = this

    new Selector[P] {
      final def apply(population: P): P = self(other(population))
    }
  }
}

trait Strength {
  val strength: Double
}

trait AdaptiveSelector[P, I] extends Serializable {
  def apply(input: I): Selector[P]
}