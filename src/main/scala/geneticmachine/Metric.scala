package geneticmachine

object Metric {
  def apply[T](metricName: String)(body: (T) => Double): Metric[T] = new Metric[T](metricName) {
    override def apply(statistics: T): Double = body(statistics)
  }
}

abstract class Metric[-T](val metricName: String) extends ((T) => Double) with Serializable


object ContinuousMetric {
  def apply[T](metricName: String)(body: (T) => Seq[Double]): ContinuousMetric[T] = new ContinuousMetric[T](metricName) {
    override def apply(statistic: T): Seq[Double] = body(statistic)
  }
}

abstract class ContinuousMetric[-T](val metricName: String) extends ((T) => Seq[Double]) with Serializable