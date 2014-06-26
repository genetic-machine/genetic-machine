package test.db.drivers

import geneticmachine.db.drivers._
import geneticmachine.ubf.UnifiedBrainFormat
import org.scalatest._
import test._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent._
import scala.concurrent.duration._

class Neo4JDriverTest(val driver: Neo4JDriver) extends FlatSpec with Matchers with BeforeAndAfterAll {

  def this() { this(new Neo4JDriver("./test-db-driver")) }

  override def afterAll() {
    driver.shutdown()
    cleanDirectory("./test-db-driver")
  }

  val ubf = UnifiedBrainFormat.sample(10)

  behavior of "Neo4JDriver"

  it must "save and load ubf properly" in {
    println(ubf)
    val id = driver.saveBrain(ubf)
    println(s"Brain Id: $id")
    val loaded = driver.loadBrain(id)
    println(loaded)
  }

  it must "be concurrent" in {
    val (t, _) = timed {
      val requests = for {
        _ <- 0 until 100
      } yield Future { driver.saveBrain(ubf) }

      val metaRequest = Future.sequence(requests)
      Await.result(metaRequest, 10.second)
    }

    println(s"Time: $t")
  }
}
