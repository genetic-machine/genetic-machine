package geneticmachine.db

import geneticmachine.db.drivers.PickleDriver

class PickleActor(val dbPath: String) extends DBActor[PickleDriver] {
  val driver = new PickleDriver(dbPath)
}
