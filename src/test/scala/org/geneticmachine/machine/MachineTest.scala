package test.machine

import akka.actor.ActorRef
import common.ViewProtocol
import org.geneticmachine.navigation.LabyrinthRobot
import org.geneticmachine.navigation.algorithm.NaiveNavigation$
import org.scalatest.{FlatSpec, BeforeAndAfterAll, Matchers}
import scala.concurrent.{Future, Await}
import scala.concurrent.duration._
import akka.pattern.ask

import org.geneticmachine._
import org.geneticmachine.machine._
import test._

class MachineTest extends FlatSpec with Matchers with BeforeAndAfterAll {

  val experimentSample = using(NaiveNavigation).startWithNew.testWith(LabyrinthRobot.sampleFactory).repeat(3)

  def executeShutdownAndCheck(machine: Machine, ex: Experiment[_, _, _, _]) {
    val results = Await.result(machine(ex), 10.seconds * ex.cycles.size)
    machine.shutdown()

    assert(results.size == ex.cycles.size)

    assert {
      results.forall { result =>
        val tr = result._1
        val tid = result._2
        tr.isSuccess && tid.isSuccess
      }
    }
  }

  override def beforeAll() {
    cleanDirectory("./genetic-machine-db")
  }

  override def afterAll() {
    cleanDirectory("./gen-db")
  }

  "Machine" must "execute experiments" in {
    val machine = new GeneticMachine() with Neo4jDB {
      override val dbPath = "./gen-db"
    }

    executeShutdownAndCheck(machine, experimentSample)
  }

  "Machine with remote control" must "execute experiments from remote console" in {
    val machine = new GeneticMachine(systemName = "GeneticServer", dbPath = "./gen-db") with Neo4jDB with RemoteControl

    val remoteMachine = remote("GeneticServer@127.0.0.1:7778")

    executeShutdownAndCheck(remoteMachine, experimentSample)

    machine.shutdown()
  }

  "Machine with remote view" must "provide read dff functionality" in {
    val machine = new GeneticMachine(systemName = "GeneticServer", dbPath = "./genetic-machine-db") with Neo4jDB with RemoteView
    val view = Await.result(machine.system.actorSelection("akka.tcp://GeneticServer@127.0.0.1:7778/user/view").resolveOne(3.seconds), 4.seconds)
    println(s"View: $view")

    val results = Await.result(machine(experimentSample), 30.seconds)

    println(results.mkString("Results:\n", "\n", "\n"))

    results.foreach { result =>
      val brainId = result._2.get
      val answer = Await.result(view.ask(ViewProtocol.GetDFF(brainId))(4.seconds), 5.seconds)
      answer match {
        case ViewProtocol.DFF(dff) =>
          println(dff)

        case _ =>
          throw new Exception("Unexpected response!")
      }
    }

    machine.shutdown()
  }

  "Machine with remote view" must "response with traverse" in {
    val machine = new GeneticMachine(systemName = "GeneticServer", dbPath = "./genetic-machine-db") with Neo4jDB with RemoteView
    val view = Await.result(machine.system.actorSelection("akka.tcp://GeneticServer@127.0.0.1:7778/user/view").resolveOne(3.seconds), 4.seconds)

    val traversed = Await.result(view.ask(ViewProtocol.Traverse(Some(0), 1000, 1000))(2.seconds), 3.seconds)

    traversed match {
      case ViewProtocol.Traversed(dff) =>
        println(dff)
        assert(dff.nodes.size == experimentSample.cycles.size * 2 + 2 + 1)
    }

    val traversedFull = Await.result(view.ask(ViewProtocol.Traverse(None, 1000, 1000))(2.seconds), 3.seconds)

    traversedFull match {
      case ViewProtocol.Traversed(dff) =>
        println(dff)
        assert(dff.nodes.size == experimentSample.cycles.size * 2 + 2 + 1)
    }

    machine.shutdown()
  }
}
