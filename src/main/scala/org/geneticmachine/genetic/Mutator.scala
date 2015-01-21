package org.geneticmachine.genetic

import scala.util.Random

trait Mutator[T] extends Serializable { self =>
  /** 0-arity mutation **/
  /** That is how the life appeared. **/
  def generation(): T

  /**
   * 0-arity mutation, 'vectorized' version.
   *
   * Allow to control generation structure, for example.
   */
  def generation(size: Int): Vector[T] = {
    Vector.fill(size)(generation())
  }

  /** 1-arity mutation **/
  /** That is how the life evolved firstly. **/
  def pointMutation(gene: T): T

  /** 1-arity mutation, 'vectorized' version. **/
  /**
   * Since there is almost only one use-case of point mutation
   *  reimplementation might speed up mutation.
   */
  def pointMutation(genes: Vector[T]): Vector[T] = {
    genes.map(pointMutation)
  }

  /** 2-arity mutation **/
  /** That is how the life grew up. **/
  def crossover(g1: T, g2: T): T

  /**
   * n-arity mutation (n > 2).
   *
   * Since our nature doesn't think there is something good in
   *  n-arity mutation when n > 2, e.g. mutation of triplets and we
   *  listen to the wisdom of billion years and so we leave n-arity
   *  mutation as forgotten by evolution function
   *  which hope to be used one day.
   *
   *  Dummy realisation simply applies crossover for each gene and randomly
   *  selected partner.
   */
  def massiveMutation(gs: Vector[T]): Vector[T] = {
    Random.shuffle(gs).zip(gs).map { genes: (T, T) =>
      val (g1, g2) = genes
      crossover(pointMutation(g1), pointMutation(g2))
    }
  }

  final def apply(): T = generation()

  final def apply(g1: T): T = pointMutation(g1)

  final def apply(g1: T, g2: T): T = crossover(g1, g2)

  final def apply(gs: Vector[T]): Vector[T] = massiveMutation(gs)

  final def asAdaptive: AdaptiveMutator[T, Any] = {
    new AdaptiveMutator[T, Any] {
      def apply(input: Any): Mutator[T] = self
    }
  }
}

/**
 * @tparam T type of genes
 * @tparam I type of input
 */
trait AdaptiveMutator[T, -I] {
  def apply(input: I): Mutator[T]
}
