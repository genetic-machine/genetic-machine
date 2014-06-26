package geneticmachine.db.drivers

import geneticmachine.ubf._

trait DBDriver {
  def saveBrain(ubf: UnifiedBrainFormat): Long
  def loadBrain(id: Long): UnifiedBrainFormat

  def shutdown(): Unit
}
