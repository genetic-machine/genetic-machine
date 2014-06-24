package test.ubf.drivers

import test._

import geneticmachine.ubf.UnifiedBrainFormat
import geneticmachine.ubf.drivers.PickleDriver
import org.scalatest._

import scala.concurrent._
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

class PickleDriverTest(val driver: PickleDriver) extends FlatSpec with Matchers with BeforeAndAfterAll{
  def this() { this(new PickleDriver("./test-pickle-driver/")) }

  val ubf = UnifiedBrainFormat.sample(0)

  behavior of "Pickle Driver"

  it must "save and load ubf properly" in {
    println(ubf)
    val saving = driver.save(ubf)
    val id = Await.result(saving, 10.second)
    println(s"Brain Id: $id")

    val loaded = Await.result(driver.load(id), 10.second)

    val number = 1000

    val (time, _) = timed {
      for {
        _ <- 0 until number
      } {
        Await.result(driver.load(id), 10.second)
      }
    }

    println(s"Time: ${time.toDouble / number} millisec per brain")
    println(loaded)
  }
}
