package org.geneticmachine.navigation.utils

import java.io.PrintWriter

import org.geneticmachine.{FinalState, ExperimentResult, PairResult}
import org.geneticmachine.navigation._

object NavigationSVG {

  implicit val scale: Int = 20

  val cellColors: CellStatus => String = {
    case CellStatus.Free => "white"
    case CellStatus.Occupied => "black"
    case CellStatus.Unknown => "grey"
  }

  abstract sealed class SVGObj {
    def x: Int
    def y: Int
    def w: Int
    def h: Int

    def asString: String = s"""<svg width="100%" height="100%">$toString</svg>"""

    def save(file: String): Unit = {
      val f = new PrintWriter(file)

      f.write(asString)

      f.close()
    }
  }

  class Rect(val x: Int, val y: Int, val color: String, val w: Int = scale, val h: Int = scale) extends SVGObj {
    override def toString: String = {
      s"""<rect x="$x" y="$y" width="$w" height="$h" fill="$color"></rect>"""
    }
  }

  object RobotPositionObj {
    def apply(rp: RobotPosition): RobotPositionObj = {
      new RobotPositionObj(rp)
    }
  }

  class RobotPositionObj(rp: RobotPosition) extends SVGObj {
    val x: Int = rp.point.x * scale
    val y: Int = rp.point.y * scale

    val w = scale
    val h = scale

    override def toString: String = {
      val points: Seq[Double] = Seq((0.15, 0.0), (0.85, 0.0), (0.5, 1.0)).map {
        case (px, py) => (px * scale + x, py * scale + y)
      }.flatMap {
        case (px, py) => Seq(px, py)
      }

      val (centerX, centerY) = (x + w / 2.0, y + h / 2.0)

      val angle = rp.direction match {
        case Direction.North => 270
        case Direction.East => 180
        case Direction.South => 90
        case Direction.West => 0
      }

      s"""<polygon points="${points.mkString(",")}" style="fill:green" transform="rotate($angle, $centerX, $centerY)"></polygon>"""
    }
  }

  def cell(x: Int, y: Int)(status: CellStatus) = new Rect(x * scale, y * scale, cellColors(status))

  object SVG {
    def vertConcat(svgs: List[SVG], space: Int = 0): SVG ={
      val cs = svgs.foldLeft((0, List.empty[SVG])) { (acc, svg) =>
        val (y, seq) = acc
        (y + svg.h + space, svg.translate(0, y) :: seq)
      }._2

      new SVG(0, 0)(cs)
    }

    def horizConcat(svgs: List[SVG], space: Int = 0): SVG = {
      val cs = svgs.foldLeft((0, List.empty[SVG])) { (acc, svg) =>
        val (x, seq) = acc
        (x + svg.w + space, svg.translate(x, 0) :: seq)
      }._2

      new SVG(0, 0)(cs)
    }

    def apply(children: Seq[SVGObj]): SVG = new SVG(0, 0)(children)
  }

  class SVG(val x: Int, val y: Int)(val children: Seq[SVGObj]) extends SVGObj {

    def translate(x: Int, y: Int): SVG = new SVG(x, y)(children)

    def updated(elems: Seq[SVGObj]) = new SVG(x, y)(children ++ elems)

    def w: Int = children.map { c =>  c.x + c.w }.max

    def h: Int = children.map { c =>  c.y + c.h }.max

    // to prevent <svg></svg> wrap
    override def asString: String = toString

    override def toString: String = {
      s"""<svg x="$x" y="$y" width="$w" height="$h">${"\n"}${children.mkString("\n")}${"\n"}</svg>"""
    }


  }

  def labyrinth(lab: Labyrinth): SVG = {
    SVG {
      lab.iterator.map {
        case ((x, y), cs) => cell(x, y)(cs)
      }.toSeq
    }
  }

  def apply(result: NavigationState): SVG = {
    val actualMap = labyrinth(result.labyrinth)

    val vision = labyrinth(result.visionMap).updated {
      result.path.map(RobotPositionObj.apply)
    }

    SVG.horizConcat(List(vision, actualMap), 20)
  }

  def apply(result: PairResult[NavigationState]): SVG = apply(result.finalState.get)

  def apply(result: FinalState[NavigationState]): SVG = apply(result.finalState)

  def apply(result: ExperimentResult[NavigationState]): SVG = {
    SVG.vertConcat(result.steps.map(NavigationSVG.apply), 25)
  }
}
