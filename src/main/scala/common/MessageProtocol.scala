package common

import geneticmachine.dataflow.DataFlowFormat

object MessageProtocol {
  abstract class Request
  abstract class Response

  case object Init extends Request

  case class Fail(e: Throwable) extends Response
  case object Ready extends Response
  case object Busy extends Response

  case class UnexpectedResponse(reason: String, msg: Any) extends Throwable(reason)
  case class InitializationFailure(e: Throwable) extends Throwable(e)

  case object Serialize extends Request
  case class Serialized(dff: DataFlowFormat) extends Response
}
