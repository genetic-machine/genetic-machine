package org.geneticmachine

import org.geneticmachine.machine.DBDriver

import scala.concurrent.{Await, Future}
import scala.concurrent.duration._

trait Machine[+C <: ExecutionContext] {
  def submit[I, O, S, C1 >: C <: ExecutionContext](experiment: Experiment[I, O, S, C1]): Future[ExperimentResult[S]]

  def apply[I, O, S, C1 >: C <: ExecutionContext](timeout: Duration)(experiment: Experiment[I, O, S, C1]): ExperimentResult[S] = {
    apply(experiment, timeout)
  }

  def apply[I, O, S, C1 >: C <: ExecutionContext](experiment: Experiment[I, O, S, C1],
                                                  timeout: Duration = 1.hour): ExperimentResult[S] = {
    val r = submit(experiment)
    Await.result(r, timeout)
  }
}

trait ServerMachine[+C <: ExecutionContext] extends Machine[C] {
  val db: DBDriver
}

trait Client[+C <: ExecutionContext] extends Machine[C]