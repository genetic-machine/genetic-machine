package test.db.drivers

import geneticmachine.db.drivers._
import common.dataflow.DataFlowFormat
import org.scalatest._
import test._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent._
import scala.concurrent.duration._

class Neo4JDriverTest(val driver: Neo4JDriver) extends FlatSpec with Matchers with BeforeAndAfterAll {

  def this() { this(new Neo4JDriver("./genetic-machine-db")) }

  override def afterAll() {
    driver.shutdown()
    cleanDirectory("./genetic-machine-db")
  }

  val dff = DataFlowFormat.sample

  behavior of "Neo4JDriver"

  it must "save and load dff properly" in {
    println(dff)
    val id = driver.save(dff)
    println(s"Brain Id: $id")
    val loaded = driver.load(id)
    println(loaded)
    assert(dff.nodes.size == loaded.nodes.size)

    def edgesCount(d: DataFlowFormat): Int = {
      (for {
        node <- d.nodes
        portConnections <- node.edges
      } yield portConnections.size).sum
    }

    assert(edgesCount(dff) == edgesCount(loaded))

  }

  it must "be concurrent" in {
    val (t, _) = timed {
      val requests = for {
        _ <- 0 until 100
      } yield Future { driver.save(dff) }

      val metaRequest = Future.sequence(requests)
      Await.result(metaRequest, 10.second)
    }

    println(s"Time: ${t.toDouble / 100} millisec per brain")
  }
}

