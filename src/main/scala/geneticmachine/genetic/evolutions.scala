package geneticmachine.genetic

import scala.collection.parallel.immutable.{ParVector, ParRange}
import scala.util.Random

object evolutions {
  final def parRange(from: Int, until: Int): ParRange = {
    from.until(until).par
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
  class SimpleEvolution[T](val populationLimit: Int, val selectionLimit: Int, val generationLimit: Int = 0)
                          (val mutator: Mutator[T], val selector: Selector[ParVector[T]])
    extends Evolution[ParVector[T]] {

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

    def apply(population: ParVector[T]): ParVector[T] = {
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

  object SimpleEvolution {
    def apply[G](populationLimit: Int, selectionLimit: Int, generationLimit: Int = 0)
                (mutator: Mutator[G], selector: Selector[ParVector[G]]): SimpleEvolution[G] = {
      new SimpleEvolution[G](populationLimit, selectionLimit, generationLimit)(mutator, selector)
    }

    def adaptive[G, I](populationLimit: Int, selectionLimit: Int, generationLimit: Int = 0) = {
      Evolution.adaptive[ParVector[G], G, I] {
        SimpleEvolution[G](populationLimit, selectionLimit, generationLimit)
      }
    }
  }

  import breeze.stats.distributions.Rand

  object SafeEvolution {
    def adaptive[G, I](populationLimit: Int, crossoverLimit: Int, mutationLimit: Int, generationLimit: Int) = {
      Evolution.adaptive[ParVector[G], G, I] {
        SafeEvolution[G](populationLimit, crossoverLimit, mutationLimit, generationLimit)
      }
    }
  }

  /**
   * Saves selected genes.
   */
  final case class SafeEvolution[T](populationLimit: Int, crossoverLimit: Int, mutationLimit: Int, generationLimit: Int)
                                   (mutator: Mutator[T], selector: Selector[ParVector[T]])
    extends Evolution[ParVector[T]] {

    def evolve(population: ParVector[T]): ParVector[T] = {
      val selected = selector(population)

      if (selected.size == 0) {
        parRange(0, populationLimit).map { _ =>
          mutator()
        }.toVector.par
      } else {
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

  case class AdaptiveSafeEvolution[T]
}
