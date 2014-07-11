package test.db.drivers

import geneticmachine.db.drivers.PickleDriver
import common.dataflow.DataFlowFormat
import org.scalatest._
import test._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Await, Future}
import scala.concurrent.duration._

class PickleDriverTest(val driver: PickleDriver) extends FlatSpec with Matchers with BeforeAndAfterAll{
  def this() { this(new PickleDriver("./test-pickle-driver/")) }

  override def afterAll() {
    driver.shutdown()
    cleanDirectory("./test-pickle-driver/")
  }

  val dff = DataFlowFormat.sample(0)

  behavior of "Pickle Driver"

  it must "save and load dff properly" in {
    println(dff)
    val id = driver.save(dff)
    println(s"Brain Id: $id")

    val loaded = driver.load(id)

    val number = 100

    val (readTime, _) = timed {
      for {
        _ <- 0 until number
      } {
        driver.load(id)
      }
    }

    val (writeTime, _) = timed {
      for {
        _ <- 0 until number
      } {
        driver.save(dff)
      }
    }

    println(s"Read time: ${readTime.toDouble / number} millisec per brain")
    println(s"Write time: ${writeTime.toDouble / number} millisec per brain")
    println(loaded)
  }

  it must "be concurrent" in {
    val number = 1000

    val saving = Future.sequence {
      for {
        _ <- 0 until number
      } yield  Future { driver.save(dff) }
    }

    val ids = Await.result(saving, (number / 10).second)

    assert(ids.toSet.size == number)
  }
}
