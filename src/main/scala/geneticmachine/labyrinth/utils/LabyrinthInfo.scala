package geneticmachine.labyrinth.utils

import breeze.linalg.DenseMatrix
import geneticmachine.RobotResult
import geneticmachine.labyrinth._

object LabyrinthInfo {
  import Direction._

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

  private val maneuverSymbol = '+'
  private val messSymbol = '*'
  private val strangeSymbol = '?'
  private val goalSymbol = 'X'

  private def pointHistoryToChar(pHist: Map[Direction, Int]): Char = {
    if (pHist.isEmpty) {
      strangeSymbol
    } else {
      val visited = pHist.map { kv: (Direction, Int) => kv._2}.sum

      val maneuver: Boolean = pHist.forall { kv: (Direction, Int) =>
        kv._2 <= 1
      }

      if (maneuver && visited < 3) {
        if (visited == 1) {
          Direction.id(pHist.keys.head)
        } else {
          maneuverSymbol
        }
      } else {
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

  def horzConcat(f1: String, f2: String, sep: String = " | "): String = {
    val ll1 = f1.lines
    val ll2 = f2.lines

    val w1 = ll1.map { _.length }.max
    val w2 = ll2.map { _.length }.max

    val ff = ll1.zipAll(ll2, "", "")
    ff.map { ll =>
      s"${ll._1}${" " * (w1 - ll._1.length)}${sep}${ll._2}${" " * (w2 - ll._2.length)}"
    }.mkString("\n")
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
  def formatInfo(result: RobotResult[LabyrinthState]): String = {
    val wState = result.worldState

    val hist = historyToDict(wState.path)

    def getFig(lab: Labyrinth): String = {
      charMatrixToString {
        val m = applyHistoryDict(labToCharMatrix(wState.labyrinth), hist)
        val goal = wState.goal
        m(goal.x, goal.y) = goalSymbol
        m
      }
    }

    val labFig = horzConcat(getFig(wState.labyrinth), getFig(wState.visionMap))

    val metrics = (for {
      (metric, value) <- result.metrics
    } yield s"$metric: value").mkString("\n")

    val cMetrics = (for {
      (metric, value) <- result.continuousMetrics
    } yield s"$metric: ${value.mkString("[", ",", "]")}").mkString("\n")

    s"Labyrinth:\n$labFig\n\nMetrics:\n$metrics\n\nContinuous metrics:\n$cMetrics"
  }

  def apply(result: RobotResult[LabyrinthState]): Unit = {
    println(formatInfo(result))
  }
}
