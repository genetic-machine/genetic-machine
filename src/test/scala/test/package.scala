import java.io._

package object test {

  def cleanDirectory(dir: String): Boolean = cleanDirectory(new File(dir))

  def cleanDirectory(dir: File): Boolean = {
    (for {
      item <- dir.listFiles()
    } yield {
      if (item.isDirectory) {
        cleanDirectory(item)
      } else {
        item.delete()
      }
    }).forall { x => x } && dir.delete()
  }

  def timed[T](x: => T): (Long, T) = {
    val start = System.currentTimeMillis()
    val result = x
    (System.currentTimeMillis() - start, result)
  }
}
