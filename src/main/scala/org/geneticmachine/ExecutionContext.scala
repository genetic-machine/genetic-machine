package org.geneticmachine

import org.geneticmachine.machine.DBDriver

import scala.concurrent.{ExecutionContext => FutureExecutionContext, Future}

trait ExecutionContext {
  implicit val futureExecutionContext: FutureExecutionContext

  def logger: org.slf4j.Logger

  def submit[I, O, S, C >: this.type <: ExecutionContext](stepPair: StepPair[I, O, S, C])
                                                         (implicit db: DBDriver): Future[PairResult[S]]
}
