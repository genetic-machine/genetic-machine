package test.ubf.drivers

import test._

import org.scalatest._

import geneticmachine.ubf.drivers.Neo4JDriver
import scala.concurrent.ExecutionContext.Implicits.global

import org.neo4j.graphdb._

import scala.collection.JavaConversions._

import scala.util._

class Neo4JPerformanceTest(val driver: Neo4JDriver) extends FlatSpec with Matchers with BeforeAndAfterAll {

  def this() { this(new Neo4JDriver("./test-db")) }

  val db = driver.graphDB

  override def afterAll() {
    driver.shutdown()
  }

  behavior of "Neo4J driver"

  val numberOfNodes = 1000
  var ids: IndexedSeq[Long] = IndexedSeq.empty[Long]

  it must "create nodes fast" in {
    val (t, ids_) = timed {
      val tx = db.beginTx()

      val nodes = for {
        i <- 0 until numberOfNodes
      } yield db.createNode()

      nodes.reduceLeft { (n1, n2) =>
        n1.createRelationshipTo(n2, Neo4JDriver.connected)
        n2
      }

      tx.success()
      tx.close()
      nodes.map { n => n.getId}
    }

    ids = ids_

    assert(t < numberOfNodes)
  }

  it must "search node fast" in {
    val (t, _) = timed {
      val tx = db.beginTx()

      ids.foreach { id =>
        assert { Try { db.getNodeById(id) }.isSuccess }
      }

      tx.success()
      tx.close()
      ids
    }

    assert(t < numberOfNodes)
  }

  it must "show nice deletion time" in {
    val (t, _) = timed {
      val tx = db.beginTx()

      ids.foreach { id =>
        val node = db.getNodeById(id)
        node.getRelationships.foreach {
          r => r.delete()
        }
        node.delete()
      }

      tx.success()
      tx.close()
    }

    assert(t < numberOfNodes)
  }

  it must "search unexistent nodes fast" in {
    val (t, _) = timed {
      val tx = db.beginTx()

      ids.foreach { id =>
        assert {
          Try {
            db.getNodeById(id)
          } match {
            case Success(x) =>
              println(x)
              false
            case Failure(_) =>
              true
          }
        }
      }

      tx.success()
      tx.close()
    }

    assert(t < numberOfNodes)
  }
}

