package org.geneticmachine.navigation.algorithm

import org.geneticmachine.navigation._
import org.geneticmachine.navigation.vision._
import org.geneticmachine.genetic._

import common.dataflow.DataFlowFormat._

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
  
  type Command = NavigationCommand.NavigationCommand

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

    s"$abstractEquation + \n${printMatrixPattern(matrix)}"
  }
}

object Gene {

  def apply(pattern: LabyrinthPattern, action: Command)
           (strength: Double, currentGain: Double): Gene = {
    new Gene(pattern, action)(strength, currentGain)
  }

  def fromProps(props: Map[String, Any]): Option[Gene] = {
    for {
      strength <- props.getAs[Double]("strength")
      currentGain <- props.getAs[Double]("gain")
      action <- props.getAs[Int]("action")
      pattern <- LabyrinthPattern.fromProps(props)
    } yield Gene(pattern, NavigationCommand(action))(strength, currentGain)
  }
}

final class Gene(val pattern: LabyrinthPattern, val action: Command)
                (val strength: Double, val currentGain: Double) extends Strength {
  def asProps: Map[String, Any] = {
    Map[String, Any](
      "strength" -> strength,
      "gain" -> currentGain,
      "action" -> action.id
    ) ++ pattern.asProps
  }

  override def toString: String = {
    s"Strength: $strength, gain: $currentGain, action: $action\n$pattern"
  }

  def withGain(updatedGain: Double): Gene = {
    Gene(pattern, action)(strength, updatedGain)
  }

  def withStrength(updatedStrength: Double): Gene = {
    Gene(pattern, action)(updatedStrength, currentGain)
  }

  def withFeedback(command: NavigationOutput)(feedback: Double): Gene = withStrength {
    val influence = if (action.id == command.id) {
      (if (currentGain > 0.0) 1.0 else 0.0) * feedback
    } else {
      0.0
    }

    strength + influence
  }
}

object LabyrinthMutator {

  def adaptive(patternR: Int, additionalCoefsLen: Int,
               learningSpeed: Double, similarityCoef: Double,
               defaultStrength: Double): Adaptation[Gene, PostObservation] = {
    new Adaptation[Gene, PostObservation] {
      def apply(input: PostObservation): Mutator[Gene] = {
        LabyrinthMutator(patternR, additionalCoefsLen, learningSpeed, similarityCoef, defaultStrength)(input)
      }
    }
  }

  val patternDistGeneration = new Gaussian(0.0, 1.0)
  val coefDistGeneration = new Gaussian(0.0, 1.0)

  val patternDistDelta = new Gaussian(0.0, 0.25)
  val coefDistDelta = new Gaussian(0.0, 0.25)

  val crossoverDist = patternDistDelta

  val actionDist = for {
    i <- Rand.randInt(NavigationCommand.maxId)
  } yield NavigationCommand(i)

  val exceptDist = Rand.randInt(NavigationCommand.maxId - 1)

  def actionDistExcept(action: NavigationCommand.NavigationCommand): NavigationCommand.NavigationCommand = {
    val a = exceptDist.draw()
    NavigationCommand {
      if (a == action.id) { NavigationCommand.maxId - 1 } else { a }
    }
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

final case class PostObservation(obs: InnerObservation, command: NavigationCommand.NavigationCommand, feedback: Double)

final case class LabyrinthMutator(patternR: Int, additionalCoefsLen: Int,
                                  learningSpeed: Double, similarityCoef: Double, defaultStrength: Double)
                                  (postObs: PostObservation)
  extends Mutator[Gene] {

  import LabyrinthMutator._

  override def generation(): Gene = {
    val currentVision = Labyrinth.copy(postObs.obs.vision, postObs.obs.from.point, patternR)(0: Int).map { _.toDouble }

    val pattern = currentVision.map { _ + patternDistGeneration.draw() }

    val coefs = postObs.obs.senses.map { s => scala.math.signum(s) + coefDistGeneration.draw() }

    val action = if (postObs.feedback >= 0.0) {
      postObs.command
    } else {
      actionDistExcept(postObs.command)
    }

    Gene(LabyrinthPattern(pattern, coefs), actionDist.draw())(defaultStrength, 0.0)
  }

  override def generation(size: Int): Vector[Gene] = {
    val currentVision = Labyrinth.copy(postObs.obs.vision, postObs.obs.from.point, patternR)(0: Int).map { _.toDouble }
    val currentSenses = postObs.obs.senses

    Vector.fill(size) {
      val pattern = currentVision.map { _ + patternDistGeneration.draw() }
      val coefs = currentSenses.map { s => scala.math.signum(s) + coefDistGeneration.draw() }
      val action = if (postObs.feedback >= 0.0) {
        postObs.command
      } else {
        actionDistExcept(postObs.command)
      }

      Gene(LabyrinthPattern(pattern, coefs), action)(defaultStrength, 0.0)
    }
  }

  def adopted(g: Gene): Gene = {
    val personalFeedback = if (g.action == postObs.command) postObs.feedback else 0.0

    val similarity = logistic(learningSpeed)(g.currentGain)

    val adaptationRatio = similarity * personalFeedback
    val noiseRatio = 1 - similarity

    val mPattern = Labyrinth.centredImpose { (v, p) =>
      p + adaptationRatio * (v - p) + noiseRatio * patternDistDelta.draw()
    }(postObs.obs.vision, postObs.obs.from.point)(g.pattern.matrix)

    val coefs = g.pattern.abstractCoefs.zip(postObs.obs.senses).map {
      case (c: Double, s: Double) =>
        c + adaptationRatio * (s - c) + noiseRatio * coefDistDelta.draw()
    }

    Gene(LabyrinthPattern(mPattern, coefs), g.action)(defaultStrength, 0.0)
  }

  override def pointMutation(g: Gene): Gene = {
    adopted(g)
  }

  override def pointMutation(gs: Vector[Gene]): Vector[Gene] = {
    gs.map(adopted)
  }

  @inline
  private def crossoverMean(d1: Double, d2: Double): Double = {
    val noise = LabyrinthMutator.crossoverDist.draw()
    (noise * (d1 - d2) + d1 + d2) / 2.0
  }

  @inline
  private def crossoverMean(d1: Double, d2: Double, s1: Double, s2: Double): Double = {
    val r1 = (LabyrinthMutator.crossoverDist.draw() + d1) * s1
    val r2 = (LabyrinthMutator.crossoverDist.draw() + d2) * s2
    (r1 + r2) / (s1 + s2)
  }

  @inline
  private def crossoverPatternRadius(r1: Int, r2: Int): Int = {
    new Poisson((r1 + r2) / 2).draw()
  }

  @inline
  private def crossoverAction(g1: Gene, g2: Gene): Command = {
    val p = g1.strength / (g2.strength + g2.strength)
    withProb(p)(g1.action, g2.action)
  }

  @inline
  private def mapPatternCrossover(g1: Gene, g2: Gene): DenseMatrix[Double] = {
    val m1 = g1.pattern.matrix: MapPattern
    val m2 = g2.pattern.matrix: MapPattern

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

    Gene(LabyrinthPattern(mapPattern, coefs), action)(defaultStrength, 0.0)
  }
}

case class Population(entities: ParVector[Gene], activeGene: Int,
                      input: Option[InnerObservation] = None, command: Option[NavigationOutput] = None)