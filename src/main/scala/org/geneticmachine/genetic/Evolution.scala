package org.geneticmachine.genetic

object Evolution {
  def adaptive[P, G, I](generator: (Mutator[G], Selector[P]) => Evolution[P])
                       (adaptiveMutator: AdaptiveMutator[G, I], adaptiveSelector: AdaptiveSelector[P, I]): AdaptiveEvolution[P, I] = {
    new AdaptiveEvolution[P, I] {
      final def apply(input: I): Evolution[P] = {
        generator(adaptiveMutator(input), adaptiveSelector(input))
      }
    }
  }
}

/**
 * `Evolution` is mutator over population in contrast to [[org.geneticmachine.genetic.Mutator]] - mutator of genes.
 *
 * Since population can be more than simply set of 'genes', all functions are defined over
 * population.
 */
trait Evolution[P] extends Serializable { self =>
  def apply(population: P): P

  final def asAdaptive[I]: AdaptiveEvolution[P, I] = {
    new AdaptiveEvolution[P, I] {
      def apply(input: I): Evolution[P] = self
    }
  }
}