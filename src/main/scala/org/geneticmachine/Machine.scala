package org.geneticmachine

import org.geneticmachine.machine.DBDriver

import scala.concurrent.Future

trait Machine[+C <: ExecutionContext] {
  def submit[I, O, S, C1 >: C](experiment: Experiment[I, O, S, C1]): Future[ExperimentResult[S]]
}

trait ServerMachine[+C <: ExecutionContext] extends Machine[C] {
  val db: DBDriver
}

trait Client[+C <: ExecutionContext] extends Machine[C]