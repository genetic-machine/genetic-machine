package geneticmachine.genetic

import scala.collection.parallel.immutable.{ParRange, ParVector}
import scala.util.Random

object Evolution {
  def adaptive[P, G, I](generator: (Mutator[G], Selector[P]) => Evolution[P])
                       (adaptation: Adaptation[G, I], adaptiveSelector: AdaptiveSelector[P, I]): AdaptiveEvolution[P, I] = {
    new AdaptiveEvolution[P, I] {
      final def apply(input: I): Evolution[P] = generator(adaptation(input), adaptiveSelector(input))
    }
  }
}

/**
 * `Evolution` is mutator over population in contrast to [[geneticmachine.genetic.Mutator]] - mutator of genes.
 *
 * Since population can be more than simply set of 'genes', all functions are defined over
 * population.
 */
trait Evolution[P] extends Serializable {
  def apply(population: P): P
}

trait AdaptiveEvolution[P, I] extends Serializable {
  def apply(input: I): Evolution[P]
}