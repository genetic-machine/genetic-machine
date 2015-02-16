package org

import java.io._
import breeze.linalg.DenseMatrix
import org.geneticmachine.navigation._
import org.scalatest._


package object geneticmachine {

  object SlowTest extends Tag("geneticmachine.test.tags.SlowTest")

  def cleanDirectory(dir: String): Boolean = cleanDirectory(new File(dir))

  def cleanDirectory(dir: File): Boolean = {
    if (dir == null) {
      true
    } else {
      dir.mkdirs()
      (for {
        item <- dir.listFiles()
      } yield {
        if (item.isDirectory) {
          cleanDirectory(item)
        } else {
          item.delete()
        }
      }).forall { x => x} && dir.delete()
    }
  }

  def timed[T](x: => T): (Long, T) = {
    val start = System.currentTimeMillis()
    val result = x
    (System.currentTimeMillis() - start, result)
  }

  def timedK(k: Int)(body: => Unit): Double = {
    val start = System.currentTimeMillis()
    (0 until k).foreach { _ =>
      body
    }

    val end = System.currentTimeMillis()
    (end - start).toDouble / k
  }

  def cellFromChar(c: Char): CellStatus = c match {
    case '#' => CellStatus.Occupied
    case '`' => CellStatus.Unknown
    case _ => CellStatus.Free
  }

  def costFromChars(labC: Seq[String]): Labyrinth = {
    val rows = labC.length
    val cols = labC(0).length

    val lab = DenseMatrix.fill[Int](rows, cols)(Int.MaxValue)

    for {
      x <- 0 until rows
      y <- 0 until cols
      if labC(x)(y).isDigit
    } {
      lab(x, y) = labC(x)(y).toString.toInt
    }
    lab
  }

  def labFromChars(labC: Seq[String]): Labyrinth = {
    val rows = labC.length
    val cols = labC(0).length

    val lab = Labyrinth.occupied(rows, cols)
    for {
      x <- 0 until rows
      y <- 0 until cols
    } {
      lab(x, y) = cellFromChar(labC(x)(y))
    }

    lab
  }
}
