
package object test {
  def timed[T](x: => T): (Long, T) = {
    val start = System.currentTimeMillis()
    val result = x
    (System.currentTimeMillis() - start, result)
  }
}
