package org.geneticmachine

import scala.concurrent.{ Future, ExecutionContext => FutureExContext }

object AbstractMetric {
  def sequence[T, R](metrics: Seq[AbstractMetric[T, R]], state: T)
                    (implicit executor: FutureExContext): Future[Map[String, R]] = {
    Future.sequence {
      for {
        metric <- metrics
      } yield Future {
        val value = metric(state)
        val name = metric.metricName

        (name, value)
      }
    }.map {
      _.toMap
    }
  }
}

abstract class AbstractMetric[-T, +R](val metricName: String) extends (T => R) with Serializable

object Metric {
  def apply[T](metricName: String)(body: (T) => Double): Metric[T] = new Metric[T](metricName) {
    override def apply(statistics: T): Double = body(statistics)
  }
}

abstract class Metric[-T](metricName: String) extends AbstractMetric[T, Double](metricName)


object ContinuousMetric {
  def apply[T](metricName: String)(body: (T) => Seq[Double]): ContinuousMetric[T] = new ContinuousMetric[T](metricName) {
    override def apply(statistic: T): Seq[Double] = body(statistic)
  }
}

abstract class ContinuousMetric[-T](metricName: String) extends AbstractMetric[T, Seq[Double]](metricName)
