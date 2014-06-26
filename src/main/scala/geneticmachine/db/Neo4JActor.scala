package geneticmachine.db

import geneticmachine.db.drivers.Neo4JDriver

class Neo4JActor(val dbPath: String) extends DBActor[Neo4JDriver] {
  val driver = new Neo4JDriver(dbPath)
}
