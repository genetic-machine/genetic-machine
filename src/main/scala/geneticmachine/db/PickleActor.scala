package geneticmachine.db

import geneticmachine.db.drivers.PickleDriver

class PickleActor(val dbPath: String) extends DBActor[PickleDriver] {
  import context.dispatcher

  val driver = new PickleDriver(dbPath)
}
