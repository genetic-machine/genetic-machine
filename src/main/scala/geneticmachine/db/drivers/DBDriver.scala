package geneticmachine.db.drivers

import common.dataflow._

trait DBDriver {
  def save(flow: DataFlowFormat): Long
  def load(id: Long): DataFlowFormat

  def traverse(startNode: Long, deep: Int,
               limit: Long, permittedConnections: Seq[String]): DataFlowFormat = {
    DataFlowFormat.empty("Traverse", "traverseInput", "traverseOutput")
  }

  def shutdown(): Unit
}
