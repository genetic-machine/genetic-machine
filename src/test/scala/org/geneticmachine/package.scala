package org

import java.io._
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
}
