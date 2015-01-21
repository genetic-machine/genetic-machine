package test.db.drivers

import org.geneticmachine.db.drivers._
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

    /** Silly equation check */
    assert(edgesCount(dff) == edgesCount(loaded))

    /** relations check */
    assert(loaded.relations == dff.relations)

    /** id injection check */
    assert(loaded(DataFlowFormat.idProp) == id)
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

  it must "traverse right" in {
    val rootId = driver.save(dff)
    val nodesNumber = 100
    (0 until nodesNumber).foldLeft(rootId) { (parentId, _) =>
      val genDff = dff.parentInjection(parentId)
      driver.save(genDff)
    }

    val nodesLimit = 25
    val depthLimit = 50
    val traversed = driver.traverse(Some(rootId), depthLimit, nodesLimit, Seq(DataFlowFormat.parentRelation))
    println(traversed)

    // limit + input and output nodes
    assert(traversed.nodes.size == nodesLimit + 2)

    val fullTraversed = driver.traverse(Some(rootId), Int.MaxValue, Long.MaxValue, Seq(DataFlowFormat.parentRelation))

    // nodes + root + input and output nodes
    assert(fullTraversed.nodes.size == nodesNumber + 1 + 2)
  }
}

