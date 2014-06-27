package geneticmachine.db.drivers

import geneticmachine.dataflow._

trait DBDriver {
  def save(flow: DataFlowFormat): Long
  def load(id: Long): DataFlowFormat

  def shutdown(): Unit
}
