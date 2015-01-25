package org.geneticmachine.navigation.utils

import breeze.linalg.DenseMatrix
import org.geneticmachine.{ExperimentResult, FinalState, PairResult}
import org.geneticmachine.navigation._
import org.geneticmachine.Experiment._

object NavigationInfo {
  private def foldHistory(hist: Seq[RobotPosition]): Map[Point, Map[Direction, Int]] = {
    def foldPoint(acc: Map[Point, Map[Direction, Int]], rp: RobotPosition): Map[Point, Map[Direction, Int]] = {
      val pointHist = if (acc.contains(rp.point)) {
        val old = acc(rp.point)
        old.updated(rp.direction, old.getOrElse(rp.direction, 0) + 1)
      } else {
        Map(rp.direction -> 1)
      }

      acc.updated(rp.point, pointHist)
    }

    hist.foldLeft(Map.empty[Point, Map[Direction, Int]])(foldPoint)
  }

  private val turnSymbol = '+'
  private val returnSymbol = '-'
  private val messSymbol = '*'
  private val strangeSymbol = '?'
  private val goalSymbol = 'X'

  private def pointHistoryToChar(pHist: Map[Direction, Int]): Char = {
    if (pHist.isEmpty) {
      strangeSymbol
    } else {
      val visited = pHist.map { kv: (Direction, Int) => kv._2}.sum

      pHist.keys.toList match {
        case d :: Nil if visited == 1 =>
          Direction.char(d)

        case d1 :: d2 :: Nil if (d1 == d2.reverse) && visited == 2 =>
          returnSymbol

        case d1 :: d2 :: Nil if (d1 <*> d2) == 0 && visited == 2 =>
          turnSymbol

        case _ =>
          messSymbol
      }
    }
  }

  private def historyToDict(hist: Seq[RobotPosition]): Map[Point, Char] = {
    val folded = foldHistory(hist)
    for {
      (p, h) <- folded
    } yield (p, pointHistoryToChar(h))
  }

  private def applyHistoryDict(m: DenseMatrix[Char], d: Map[Point, Char]): DenseMatrix[Char] = {
    for ((p, c) <- d) {
      m(p.x, p.y) = c
    }

    m
  }

  def horzConcat(header1: String = "", header2: String = "")(f1: String, f2: String, sep: String = "  |  "): String = {

    def center(s: String, w: Int): String = {
      val wLeft = (w - s.size) / 2
      s"${" " * wLeft}$s${" " * (w - s.size - wLeft)}"
    }

    val ll1 = f1.lines.toList
    val ll2 = f2.lines.toList

    val w1 = (header1 :: ll1).map { _.length }.max
    val w2 = (header2 :: ll2).map { _.length }.max

    val ff = ll1.zipAll(ll2, "", "")

    val header = s"${center(header1, w1)}$sep${center(header2, w2)}"

    val splitter = s"${" " * w1}$sep${" " * w2}"

    val body = ff.map { ll =>
      s"${ll._1}${" " * (w1 - ll._1.length)}$sep${ll._2}${" " * (w2 - ll._2.length)}"
    }.mkString("\n")

    s"$header\n$splitter\n$body"
  }

  def getFig(lab: Labyrinth, hist: List[RobotPosition], goal: Point): String = {
    val h = historyToDict(hist)

    charMatrixToString {
      val m = applyHistoryDict(labToCharMatrix(lab), h)
      m(goal.x, goal.y) = goalSymbol
      m
    }
  }

  def formatNavigationState(state: NavigationState): String = {
    val hist = historyToDict(state.path)

    def getFig(lab: Labyrinth): String = {
      charMatrixToString {
        val m = applyHistoryDict(labToCharMatrix(lab), hist)
        val goal = state.goal
        m(goal.x, goal.y) = goalSymbol
        val rp = state.robotPosition.point
        m(rp.x, rp.y) = Direction.char(state.robotPosition.direction)
        m
      }
    }

    horzConcat("Actual map", "Vision map")(getFig(state.labyrinth), getFig(state.visionMap))
  }

  /**
   * Format results of a single experiment in following style:
   * {{{
   *   Labyrinth:
   *   <labyrinth> <vision map>
   *
   *   Metrics:
   *   <metrics>
   *
   *   Continuous metrics:
   *   <continuous metrics>
   * }}}
   *
   */
  def formatInfo(result: FinalState[NavigationState]): String = {
    val labFig = formatNavigationState(result.finalState)

    val metrics = (for {
      (metric, value) <- result.metrics
    } yield s"$metric: $value").mkString("\n")

    val cMetrics = (for {
      (metric, values) <- result.continuousMetrics
      fValues = values.map { v => "%1.2f".format(v) }.mkString("[", ",", "]")
    } yield s"$metric: $fValues").mkString("\n")

    s"Labyrinth:\n$labFig\n\nMetrics:\n$metrics\n\nContinuous metrics:\n$cMetrics"
  }

  private def formatInfo(result: PairResult[NavigationState]): String = {
    val PairResult(rr, id) = result
    val idInfo  = id.map{ _.toString }.recover { case t => t.toString }.get
    val rrInfo = rr.map(formatInfo).recover { case t => t.toString }.get

    s"Cycle result [$idInfo]:\n$rrInfo"
  }

  def formatInfo(result: ExperimentResult[NavigationState]): String = {
    result.steps.map(formatInfo).mkString("\n\n")
  }

  def apply(result: PairResult[NavigationState]): String = {
    formatInfo(result)
  }

  def apply(result: ExperimentResult[NavigationState]): String = {
    formatInfo(result)
  }
}
