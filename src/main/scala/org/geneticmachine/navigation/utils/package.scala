package org.geneticmachine.navigation

package object utils {
  def logistic(alpha: Double)(x: Double): Double = {
    1.0 / (1 + math.exp(-x * alpha))
  }

  def transpose[A](xss: List[List[A]], default: A): List[List[A]] = {
    def tr(xss: List[List[A]], default: A, acc: List[List[A]]): List[List[A]] = {
      if (xss.forall { xs => xs.isEmpty }) {
        acc
      } else {
        val row = xss.map { xs => xs.headOption.getOrElse(default)}
        val rest = xss.map { case Nil => Nil; case ys => ys.tail}
        tr(rest, default, row :: acc)
      }
    }

    tr(xss, default, List.empty).reverse
  }

  def left(w: Int, s: String): String = {
    s"$s${" " * (w - s.size)}"
  }

  def width(row: List[String], sepW: Int): Int = {
    row.map { _.size }.sum + (row.size - 1) * sepW
  }

  case class Figure(header: String, lines: List[String]) {
    def this(header: String, str: String) = this(header, str.split("\n").toList)

    def width: Int = header.size max lines.map { _.size }.max

    def |(other: Figure): Figure = {
      val w1 = this.width
      val w2 = other.width

      val h = left(w1, this.header) + " | " + left(w2, other.header)
      val ls = this.lines.zipAll(other.lines, "", "").map {
        case (l1, l2) =>
          left(w1, l1) + " | " + left(w2, l2)
      }

      Figure(h, ls)
    }

    override def toString: String = {
      val w = width
      left(w, header) + "\n" +
        ("-" * w) + "\n" +
        lines.map { l => left(w, l) }.mkString("\n")
    }
  }

  def horizConcat(headers: Seq[String], figs: Seq[String], sep: String = "  |  "): String = {
    headers.zip(figs).map { case (h, f) => Figure(h, f.split("\n").toList) }.reduce { _ | _ }.toString
  }
}
