package org.geneticmachine

import org.geneticmachine.machine.DBDriver

import scala.concurrent.{ExecutionContext => FutureExecutionContext, Future}

trait ExecutionContext {
  type C <: ExecutionContext

  implicit val futureExecutionContext: FutureExecutionContext

  def logger: org.slf4j.Logger

  def submit[I, O, S, C1 >: this.type](stepPair: StepPair[I, O, S, C1])(implicit db: DBDriver): Future[PairResult[S]]
}
