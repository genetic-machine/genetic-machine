package geneticmachine.labyrinth.brain

import geneticmachine.labyrinth._
import geneticmachine.labyrinth.brain.InnerObservation
import geneticmachine.labyrinth.vision._
import geneticmachine.genetic._

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
  
  type Command = LabyrinthCommand.LabyrinthCommand

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

  def compare(obs: VisionObservation, abstractSignal: Array[Double]): Double = {
    ((matrix: MapPattern) * obs + convolution(abstractCoefs, abstractSignal)) / statSum
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
    } yield s"$x s$i").mkString(" + ")

    s"$abstractEquation + \n${matrix.map {c: Double => (c * 10.0).round / 10.0 } }"
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
    } yield Gene(pattern, LabyrinthCommand(action))(strength, currentGain)
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
}

object AdaptiveLabyrinthMutator {
  val patternDist = new Gaussian(0.0, 1.0)
  val coefDist = new Gaussian(0.0, 1.0)

  val crossoverDist = patternDist

  val actionDist = for {
    i <- Rand.randInt(LabyrinthCommand.maxId)
  } yield LabyrinthCommand(i)

  val exceptDist = Rand.randInt(LabyrinthCommand.maxId - 1)

  def actionDistExcept(action: LabyrinthCommand.LabyrinthCommand): LabyrinthCommand.LabyrinthCommand = {
    val a = exceptDist.draw()
    LabyrinthCommand {
      if (a == action.id) { LabyrinthCommand.maxId } else { a }
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

final case class PostObservation(obs: InnerObservation, command: LabyrinthCommand.LabyrinthCommand, feedback: Double)

final case class AdaptiveLabyrinthMutator(patternR: Int, additionalCoefsLen: Int, learningSpeed: Double)
                                         (postObs: PostObservation)
  extends Mutator[Gene] {

  import AdaptiveLabyrinthMutator._

  override def generation(): Gene = {
    val currentVision = Labyrinth.copy(postObs.obs.vision, postObs.obs.from.point, patternR)(0: Int).map { _.toDouble }

    val pattern = currentVision.map { _ + patternDist.draw() }

    val coefs = postObs.obs.senses.map { s => scala.math.signum(s) + coefDist.draw() }

    val action = if (postObs.feedback >= 0.0) {
      postObs.command
    } else {
      actionDistExcept(postObs.command)
    }

    Gene(LabyrinthPattern(pattern, coefs), action)(0.0, 0.0)
  }

  def adopt(g: Gene): Gene = {
    val pattern = g.pattern.matrix

    val similarity = logistic(learningSpeed)(g.currentGain)

    val mPattern = Labyrinth.centredImpose { (v, p) =>
      p + (v - p) * similarity * postObs.feedback + (1 - similarity) * patternDist.draw()
    }(postObs.obs.vision, postObs.obs.from.point)(pattern)

    val coefs = g.pattern.abstractCoefs.zip(postObs.obs.senses) { case (c: Double, s: Double) =>
        c + similarity * (math.signum(s) - c) * postObs.feedback + (1 - similarity) * coefDist.draw()
    }

    Gene(LabyrinthPattern(mPattern, coefs), g.action)(0.0, 0.0)
  }

  /**
   * This method __isn't__ equal to the {{{gs.map(pointMutation)}}}
   *
   * Probability of command (both pattern command and action) mutation depends on __normed__ strength:
   * `P`,,change,, = 2/3 `exp`(-`strength` / `strength`,,mean,,)
   * produced by gamma distribution with `shape`, `scale` = 1.
   *
   * Map pattern mutates by adding a normally distributed noise.
   *
   * Command coefficient `cc` mutates by gamma distribution:
   * `cc`,,new,, = `cc` * `gamma`(1, 1)
   */
  override def pointMutation(gs: Vector[Gene]): Vector[Gene] = {
    val strengthMean = gs.map { _.strength }.sum / gs.size
    gs.map { g: Gene =>
      pointMutation(g, g.strength / strengthMean)
    }
  }

  @inline
  private def crossoverMean(d1: Double, d2: Double): Double = {
    val noise = AdaptiveLabyrinthMutator.crossoverDist.draw()
    (noise * (d1 - d2) + d1 + d2) / 2.0
  }

  @inline
  private def crossoverPatternRadius(r1: Int, r2: Int): Int = {
    new Poisson((r1 + r2) / 2).draw()
  }

  @inline
  private def crossoverCommand(c1: Command, c2: Command): Command = {
    if (c1 == c2) {
      c1
    } else {
      if (Rand.randInt(2).draw() == 0) { c1 } else { c2 }
    }
  }

  @inline
  private def mapPatternCrossover(matrix1: DenseMatrix[Double],
    matrix2: DenseMatrix[Double]): DenseMatrix[Double] = {
    val m1 = matrix1: MapPattern
    val m2 = matrix2: MapPattern

    val r1 = m1.patternRadius.lInfNorm // max size, just in case
    val r2 = m2.patternRadius.lInfNorm
    val r = crossoverPatternRadius(r1, r2)

    val crossoveredMatrix = MapPattern.fill(r, 0)

    for(i <- -r to r; j <- -r to r) {
      val x1 = m1(i, j)
      val x2 = m2(i, j)
      crossoveredMatrix(i, j) = crossoverMean(x1, x2)
    }

    crossoveredMatrix.matrix
  }

  override def crossover(g1: Gene, g2: Gene): Gene = {
    val mapPattern = mapPatternCrossover(g1.pattern.matrix, g2.pattern.matrix)
    val patternCommand = crossoverCommand(g1.pattern.patternCommand, g2.pattern.patternCommand)
    val patternCommandCoef = crossoverMean(g1.pattern.commandCoefficient, g2.pattern.commandCoefficient)
    val action = crossoverCommand(g1.action , g2.action)

    Gene(LabyrinthPattern(mapPattern, patternCommand, patternCommandCoef), (g1.strength + g2.strength) / 2.0, 0.0, action)
  }
}

case class Population(entities: ParVector[Gene], activeGene: Int)