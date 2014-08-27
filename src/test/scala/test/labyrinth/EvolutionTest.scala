package test.labyrinth

import akka.actor.{Props, ActorSystem}
import akka.testkit.TestKit
import common.MessageProtocol
import geneticmachine.Experiment._
import geneticmachine.ExperimentActor
import geneticmachine.db.Neo4JActor
import geneticmachine.labyrinth.brain.DijkstraBrain
import geneticmachine.labyrinth.{LabyrinthRobot, LabyrinthStatus}
import org.scalatest._
import akka.pattern.ask
import scala.concurrent.Await
import scala.concurrent.duration._

import test._

import scala.util.Try

class EvolutionTest(_system: ActorSystem) extends TestKit(_system)
  with WordSpecLike with Matchers with BeforeAndAfterAll {

  def this() { this(ActorSystem("test-system")) }

  override def beforeAll() {
    cleanDirectory("./genetic-machine-db")
  }

  override def afterAll() {
    _system.shutdown()
  }

  "Evolution" must {
    "work" in {
      val dbActor = _system.actorOf(Props(new Neo4JActor("./genetic-machine-db")))

      val rf = LabyrinthRobot.sampleFactory
      val experiment = using(DijkstraBrain).startWithNew.testWith(rf).repeat(3)

      val eActor = _system.actorOf(Props(new ExperimentActor(experiment, dbActor)), "Experiment")
      Await.result(eActor.ask(MessageProtocol.Init)(60.seconds), 62.seconds) match {
        case ExperimentActor.ExperimentResult(rs: List[(Try[LabyrinthStatus], Try[Long])]) =>
          println(s"Results:\n${rs.mkString("\n")}")
          assert (rs.forall { (r) => r._1.isSuccess })

        case msg =>
          println(s"Strange response: $msg")
          throw new Exception
      }
    }
  }
}
