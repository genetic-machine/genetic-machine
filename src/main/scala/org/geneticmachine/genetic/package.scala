package org.geneticmachine

package object genetic {
  type AdaptiveMutator[T, -I] = I => Mutator[T]

  type AdaptiveEvolution[P, I] = I => Evolution[P]
}
