package common

import common.dataflow.DataFlowFormat

object ViewProtocol {
   import common.MessageProtocol._

   case class GetDFF(id: Long) extends Request
   case class Traverse(id: Option[Long], depth: Int, limit: Long) extends Request

   case class DFF(dff: DataFlowFormat) extends Response
   case class Traversed(dff: DataFlowFormat) extends Response
 }
