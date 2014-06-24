package test.ubf.drivers

import org.scalatest._
import geneticmachine.ubf.drivers._
import geneticmachine.ubf.UnifiedBrainFormat

import scala.concurrent._
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

class Neo4JDriverTest(val driver: Neo4JDriver) extends FlatSpec with Matchers with BeforeAndAfterAll {

  def this() { this(new Neo4JDriver("./test-db-driver")) }

  override def afterAll() {
    driver.shutdown()
  }

  val ubf = UnifiedBrainFormat.sample(10)

  behavior of "Neo4JDriver"

  it must "save and load ubf properly" in {
    println(ubf)
    val saving = driver.save(ubf)
    val id = Await.result(saving, 1.second)
    println(s"Brain Id: $id")
    val loaded = Await.result(driver.load(id), 1.second)
    println(loaded)
  }
}