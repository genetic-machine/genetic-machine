package geneticmachine.genetic

import scala.collection.parallel.immutable.{ParRange, ParVector}
import scala.util.Random

trait Mutator[T] extends Serializable {
  /** 0-arity mutation **/
  /** That is how the life appeared. **/
  def generation(): T

  /** 1-arity mutation **/
  /** That is how the life evolved firstly. **/
  def pointMutation(gene: T): T

  /** 1-arity mutation, 'vectorized' version. **/
  /** Since there is almost only one use-case of point mutation
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
}

/**
 * Represents selection.
 */
trait Selector[P] extends Serializable {
  final def apply(population: P): P = select(population)

  def select(population: P): P
}

/**
 * `Evolution` is mutator over population in contrast to [[geneticmachine.genetic.Mutator]] - mutator of genes.
 *
 * Since population can be more than simply set of 'genes', all functions are defined over
 * population.
 */
trait Evolution[P] extends Serializable {

  final def apply(population: P): P = evolve(population)

  def evolve(population: P): P
}

object Evolution {
  final def parRange(from: Int, until: Int): ParRange = {
    from.until(until).par
  }
}

/**
 * Simple realisation of evolution process. Each gene passed the selection step is guarantied to be used
 * during crossover at least once.
 *
 * Keep population in size `populationLimit`.
 * Step 1: selection.
 * Step 2: point mutation.
 * Step 3: produces up to `populationLimit` but not less than `populationLimit - generationLimit` genes by crossover;
 * it's guarantied each gene is used in crossover at least once.
 * Step 4: fill up to `populationLimit` by generation of new genes.
 */
class SimpleEvolution[T]
    (val mutator: Mutator[T], val selector: Selector[ParVector[T]],
     val populationLimit: Int, val selectionLimit: Int, val generationLimit: Int = 0)
  extends Evolution[ParVector[T]] {

  import Evolution._

  final def crossover(gs: ParVector[T]): ParVector[T] = {
    val toFill = populationLimit - generationLimit - gs.size

    /** To guarantee every genetic material is in use, i.e. each gene produces child. **/
    val mainGeneration = for {
      g1 <- gs
      i = Random.nextInt(gs.size)
      g2 = gs(i)
    } yield mutator(g1, g2)

    val additionalGeneration = if (gs.nonEmpty) {
      for {
        _ <- parRange(0, toFill)
        i = Random.nextInt(gs.size)
        g1 = gs(i)
        j = Random.nextInt(gs.size)
        g2 = gs(j)
      } yield mutator(g1, g2)
    } else {
      Vector.empty[T]
    }

    mainGeneration ++ additionalGeneration
  }

  def evolve(population: ParVector[T]): ParVector[T] = {
    val crossovered = crossover {
      selector(population).map { gene: T =>
        mutator(gene)
      }
    }

    val generated = parRange(0, populationLimit - crossovered.size).map { _ =>
      mutator()
    }

    crossovered ++ generated
  }
}

import breeze.stats.distributions.Rand

/**
 * Saves selected genes.
 */
final class SafeEvolution[T] (val mutator: Mutator[T], val selector: Selector[ParVector[T]],
                              val populationLimit: Int, val crossoverLimit: Int,
                              val mutationLimit: Int, val generationLimit: Int)
  extends Evolution[ParVector[T]] {

  import Evolution._

  def evolve(population: ParVector[T]): ParVector[T] = {
    if (population.isEmpty) {
      parRange(0, populationLimit).map { _ =>
        mutator()
      }.toVector.par
    } else {
      val selected = selector(population)

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