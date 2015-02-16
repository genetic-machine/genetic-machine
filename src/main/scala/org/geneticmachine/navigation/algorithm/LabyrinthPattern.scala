package org.geneticmachine.navigation.algorithm

import org.geneticmachine.navigation._
import org.geneticmachine.navigation.vision._
import org.geneticmachine.genetic._

import org.geneticmachine.common.graph.Graph._

import breeze.linalg.DenseMatrix
import breeze.stats.distributions._

import scala.collection.parallel.immutable.ParVector

object LabyrinthPattern {
  def fromProps(props: Map[String, Any]): Option[LabyrinthPattern] = {
    for {
      flatMatrix <- props.getAs[Array[Double]]("matrix")
      rows <- props.getAs[Int]("rows")
      cols <- props.getAs[Int]("cols")
      coefs <- props.getAs[Array[Double]]("coefs")
      matrix = matrixFromArray(flatMatrix, rows, cols)
    } yield LabyrinthPattern(matrix, coefs)
  }

  /**
   * Vector multiplication: c' * v.
   * If some values or coefs are missing, they are assumed to be zero.
   */
  def convolution(coefs: Array[Double], values: Array[Double]): Double = {
    (for {
      (c, v) <- coefs.zip(values)
    } yield c * v).sum
  }
}

import LabyrinthPattern._

final case class LabyrinthPattern(matrix: DenseMatrix[Double], abstractCoefs: Array[Double]) {

  val statSum = 1.0 + abstractCoefs.map(math.abs).sum

  def compare(obs: InnerObservation): Double = {
    val mapConvolution = (matrix: MapPattern) * VisionObservation(obs.vision, obs.from)
    val signalConvolution =  convolution(abstractCoefs, obs.senses)

    (mapConvolution + signalConvolution) / statSum
  }

  def asProps: Map[String, Any] = {
    val (flatMatrix, rows, cols) = matrixToArray(matrix)
    Map[String, Any] (
      "matrix" -> flatMatrix,
      "rows" -> rows,
      "cols" -> cols,
      "coefs" -> abstractCoefs
    )
  }

  override def toString: String = {
    val abstractEquation = (for {
      (x, i) <- abstractCoefs.zipWithIndex
    } yield s"${"%.1f" format x} s$i").mkString(" + ")

    s"$abstractEquation + \n${greyMap(matrix, normalize = false)}"
  }
}

object Gene {

  def apply(pattern: LabyrinthPattern, action: NavigationCommand)
           (strength: Double, currentGain: Double): Gene = {
    new Gene(pattern, action)(strength, currentGain)
  }

  def fromProps(props: Map[String, Any]): Option[Gene] = {
    for {
      strength <- props.getAs[Double]("strength")
      currentGain <- props.getAs[Double]("gain")
      action <- props.getAs[NavigationCommand]("action")
      pattern <- LabyrinthPattern.fromProps(props)
    } yield Gene(pattern, action)(strength, currentGain)
  }
}

final class Gene(val pattern: LabyrinthPattern, val action: NavigationCommand)
                (val strength: Double, val currentGain: Double) extends GeneStrength {
  def asProps: Map[String, Any] = {
    Map[String, Any](
      "strength" -> strength,
      "gain" -> currentGain,
      "action" -> action
    ) ++ pattern.asProps
  }

  override def toString: String = {
    s"Strength: $strength, gain: $currentGain, action: ${NavigationCommand.char(action)}\n$pattern"
  }

  def withGain(updatedGain: Double): Gene = {
    Gene(pattern, action)(strength, updatedGain)
  }

  def withStrength(updatedStrength: Double): Gene = {
    Gene(pattern, action)(updatedStrength, currentGain)
  }

  def withFeedback(command: NavigationOutput)(feedback: Double): Gene = withStrength {
    val influence = if (action == command) {
      (if (currentGain > 0.0) 1.0 else 0.0) * feedback
    } else {
      0.0
    }

    strength + influence
  }
}

trait MutatorParams {
  val additionalCoefsLen: Int

  val patternR: Int = 3
  val patternRD: Int = 1

  val learningSpeed: Double = 0.01
  val similarityCoefs: Double = 0.1

  val defaultStrength: Double = 0.1

  val generationPatternD: Double = 0.5
  val generationCoefsD: Double = 0.5

  val adaptationPatternD: Double = 0.05
  val adaptationCoefsD: Double = 0.05
}

object FusionMutator {
  def adaptive(params: MutatorParams): AdaptiveMutator[Gene, PostObservation] = {
        FusionMutator(params)
  }

  val patternDistGeneration = new Gaussian(0.0, 1.0)
  val coefDistGeneration = new Gaussian(0.0, 1.0)

  val patternDistDelta = new Gaussian(0.0, 1.0)
  val coefDistDelta = new Gaussian(0.0, 1.0)

  val crossoverDist = patternDistDelta

  val actionDist = for {
    i <- Rand.randInt(NavigationCommand.max)
  } yield i

  val exceptDist = Rand.randInt(NavigationCommand.max)

  def actionDistExcept(action: NavigationCommand): NavigationCommand = {
    val a: Int = exceptDist.draw()

    if (a == action) { NavigationCommand.max - 1 } else { a }
  }

  private val uniform = new Uniform(0.0, 1.0)
  private val gamma = new Gamma(1.0, 1.0)

  def withProb[A](p: Double)(left: A, right: A): A = {
    if (uniform.draw() < p) {
      left
    } else {
      right
    }
  }

  def withGammaProb[A](p: Double)(left: A, right: A): A = {
    if (gamma.draw() < p) {
      left
    } else {
      right
    }
  }

  def logistic(alpha: Double)(x: Double): Double = {
    1.0 / (1.0 + math.exp(- x * alpha))
  }
}

final case class PostObservation(obs: InnerObservation, command: NavigationCommand, feedback: Double)

final case class FusionMutator(params: MutatorParams)(postObs: PostObservation)
  extends Mutator[Gene] {

  import FusionMutator._

  val additionalCoefsLen: Int = params.additionalCoefsLen

  val patternR: Int = params.patternR
  val patternRD: Int = params.patternRD

  val learningSpeed: Double = params.learningSpeed
  val similarityCoefs: Double = params.similarityCoefs

  val defaultStrength: Double = params.defaultStrength

  val generationPatternD: Double = params.generationPatternD
  val generationCoefsD: Double = params.generationCoefsD

  val adaptationPatternD: Double = params.adaptationPatternD
  val adaptationCoefsD: Double = params.adaptationCoefsD

  override def generation(): Gene = {
    val currentVision = Labyrinth.copy(postObs.obs.vision, postObs.obs.from.point, patternR)(0: Int).map { _.toDouble }

    val pattern = currentVision.map { _ + patternDistGeneration.draw() * generationPatternD }

    val coefs = postObs.obs.senses.map { _ + coefDistGeneration.draw() * generationCoefsD }

    val action = if (postObs.feedback >= 0.0) {
      postObs.command
    } else {
      actionDistExcept(postObs.command)
    }

    Gene(LabyrinthPattern(pattern, coefs), action)(defaultStrength, 0.0)
  }

  override def generation(size: Int): Vector[Gene] = {
    val currentVision = Labyrinth.copy(postObs.obs.vision, postObs.obs.from.point, patternR)(0: Int).map { _.toDouble }
    val currentSenses = postObs.obs.senses

    Vector.fill(size) {
      val pattern = currentVision.map { _ + patternDistGeneration.draw() * generationPatternD }
      val coefs = currentSenses.map { _ + coefDistGeneration.draw() * generationCoefsD }

      val action = if (postObs.feedback >= 0.0) {
        postObs.command
      } else {
        actionDistExcept(postObs.command)
      }

      Gene(LabyrinthPattern(pattern, coefs), action)(defaultStrength, 0.0)
    }
  }

  def adapted(g: Gene): Gene = {
    val personalFeedback = if (g.action == postObs.command) postObs.feedback else 0.0
    val obs = postObs.obs

    val similarity = logistic(learningSpeed)(g.currentGain)

    val mPattern = Labyrinth.centredImpose { (v, p) =>
      p + (similarity * (v - p) + (1 - similarity) * patternDistDelta.draw()) * adaptationPatternD
    }(obs.vision, obs.from.point)(g.pattern.matrix)

    val coefs = g.pattern.abstractCoefs.zip(postObs.obs.senses).map { case (c, s) =>
      c + (similarity * (s - c) + (1 - similarity) * coefDistDelta.draw()) * adaptationCoefsD
    }

    Gene(LabyrinthPattern(mPattern, coefs), g.action)(defaultStrength, 0.0)
  }

  override def pointMutation(g: Gene): Gene = {
    adapted(g)
  }

  override def pointMutation(gs: Vector[Gene]): Vector[Gene] = {
    gs.map(adapted)
  }

  @inline
  private def crossoverMean(d1: Double, d2: Double): Double = {
    val noise = FusionMutator.crossoverDist.draw()
    (noise * (d1 - d2) + d1 + d2) / 2.0
  }

  @inline
  private def crossoverMean(d1: Double, d2: Double, s1: Double, s2: Double): Double = {
    val r1 = (FusionMutator.crossoverDist.draw() + d1) * s1
    val r2 = (FusionMutator.crossoverDist.draw() + d2) * s2
    (r1 + r2) / (s1 + s2)
  }

  @inline
  private def crossoverPatternRadius(r1: Int, r2: Int): Int = {
    new Poisson((r1 + r2) / 2).draw()
  }

  @inline
  private def crossoverAction(g1: Gene, g2: Gene): NavigationCommand = {
    val p = g1.strength / (g2.strength + g2.strength)
    withProb(p)(g1.action, g2.action)
  }

  @inline
  private def mapPatternCrossover(g1: Gene, g2: Gene): DenseMatrix[Double] = {
    val m1: MapPattern = g1.pattern.matrix
    val m2: MapPattern = g2.pattern.matrix

    val r1 = m1.patternRadius.lInfNorm // max size, just in case
    val r2 = m2.patternRadius.lInfNorm
    val r = crossoverPatternRadius(r1, r2)

    val crossoveredMatrix = MapPattern.fill(r, 0)

    for(i <- -r to r; j <- -r to r) {
      val x1 = m1(i, j)
      val x2 = m2(i, j)
      crossoveredMatrix(i, j) = crossoverMean(x1, x2, g1.strength, g2.strength)
    }

    crossoveredMatrix.matrix
  }

  override def crossover(g1: Gene, g2: Gene): Gene = {
    val mapPattern = mapPatternCrossover(g1, g2)
    val coefs = g1.pattern.abstractCoefs.zip(g2.pattern.abstractCoefs).map {
      case (c1, c2) => crossoverMean(c1, c2, g1.strength, g2.strength)
    }

    val action = crossoverAction(g1 , g2)

    Gene(LabyrinthPattern(mapPattern, coefs), action)(params.defaultStrength, 0.0)
  }
}

case class Population(entities: ParVector[Gene], activeGene: Int,
                      input: Option[InnerObservation] = None, command: Option[NavigationOutput] = None)