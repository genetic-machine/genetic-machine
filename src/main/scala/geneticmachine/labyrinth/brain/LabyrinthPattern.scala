package geneticmachine.labyrinth.brain

import geneticmachine.labyrinth._
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
      commandCoefficient <- props.getAs[Double]("cc")
      patternCommand <- props.getAs[Int]("pc")
      matrix = matrixFromArray(flatMatrix, rows, cols)
    } yield LabyrinthPattern(matrix, LabyrinthCommand(patternCommand), commandCoefficient)
  }
  
  type Command = LabyrinthCommand.LabyrinthCommand
}

import LabyrinthPattern._

final case class LabyrinthPattern(matrix: DenseMatrix[Double],
                                  patternCommand: Command, commandCoefficient: Double) {

  def compare(obs: Observation, commandSignal: CommandSignal): Double = {
    (matrix: MapPattern) * obs + commandCoefficient * commandSignal(patternCommand)
  }

  def asProps: Map[String, Any] = {
    val (flatMatrix, rows, cols) = matrixToArray(matrix)
    Map[String, Any] (
      "matrix" -> flatMatrix,
      "rows" -> rows,
      "cols" -> cols,
      "cc" -> commandCoefficient,
      "pc" -> patternCommand.id
    )
  }

  override def toString: String = {
    s"$commandCoefficient * $patternCommand + \n${matrix.map {c: Double => (c * 10.0).round / 10.0 } }"
  }
}

final case class Gene(pattern: LabyrinthPattern, strength: Double,
                      currentGain: Double, action: Command) extends Strength {
  def asProps: Map[String, Any] = {
    Map[String, Any](
      "strength" -> strength,
      "current" -> currentGain,
      "command" -> action.id
    ) ++ pattern.asProps
  }

  override def toString: String = {
    s"Strength: $strength, gain: $currentGain, action: $action:\n$pattern"
  }
}

object Gene {
  def fromProps(props: Map[String, Any]): Option[Gene] = {
    for {
      strength <- props.getAs[Double]("strength")
      currentGain <- props.getAs[Double]("current")
      command <- props.getAs[Int]("command")
      pattern <- LabyrinthPattern.fromProps(props)
    } yield Gene(pattern, strength, currentGain, LabyrinthCommand(command))
  }
}

object LabyrinthPatternMutator {
  val patternDist = new Gaussian(0.0, 1.0)

  val crossoverDist = patternDist

  val actionDist = for {
    i <- Rand.randInt(LabyrinthCommand.maxId)
  } yield LabyrinthCommand(i)

  val commandValueDist = new Gamma(1.0, 1.0)

  private val uniform = new Uniform(0.0, 1.0)
  private val gamma = new Gamma(1.0, 1.0)

  def withProb[A](p: Double)(left: A, right: A): A = {
    if (uniform.draw() < p) {
      left
    } else {
      right
    }
  }

  /** Seems to be faster than x: => A for simple x */
  def withGammaProb[A](p: Double)(left: A, right: A): A = {
    if (gamma.draw() < p) {
      left
    } else {
      right
    }
  }
}

final case class LabyrinthPatternMutator(patternR: Int) extends Mutator[Gene] {

  import LabyrinthPatternMutator._

  override def generation(): Gene = {
    val pattern = DenseMatrix.zeros[Double](2 * patternR + 1, 2 * patternR + 1).map { _ =>
      patternDist.draw()
    }

    val patternCommand = actionDist.draw()
    val commandValue = commandValueDist.draw()

    val action = actionDist.draw()

    Gene(LabyrinthPattern(pattern, patternCommand, commandValue), 0.0, 0.0, action)
  }

  private def pointMutation(g: Gene, strength: Double): Gene = {
    val pattern = g.pattern.matrix.copy
    pattern.map { c: Double =>
      c + patternDist.draw()
    }

    val patternCommandC = g.pattern.commandCoefficient * commandValueDist.draw()
    val patternCommand = withGammaProb(g.strength)(g.pattern.patternCommand, actionDist.draw())

    val action = withGammaProb(g.strength)(g.action, actionDist.draw())

    Gene(LabyrinthPattern(pattern, patternCommand, patternCommandC), 1.0, 0.0, action)
  }

  /** This method requires careful usage
   *  since mutation of `patternCommand` and `action` depends on __unnormed__ `strength` of the gene.
   */
  override def pointMutation(g: Gene): Gene = {
    pointMutation(g, g.strength)
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
    val noise = LabyrinthPatternMutator.crossoverDist.draw()
    //Gaussian with mean = (d1 + d2) / 2 and std = (d1 - d2) / 2.0
    // this shouldn't loose precision... Anyway it uses random, so what is about precision again?
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