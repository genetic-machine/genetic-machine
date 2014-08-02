package common

import common.MessageProtocol._
import common.dataflow.DataFlowFormat

object ViewProtocol {

  case class DFF(dff: DataFlowFormat) extends Response
  case class Traversed(dff: DataFlowFormat) extends Response

  case class GetDFF(id: Long) extends Request
  case class Traverse(id: Option[Long], depth: Int, limit: Long) extends Request
 }
