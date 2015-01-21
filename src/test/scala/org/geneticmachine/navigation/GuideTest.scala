package test.labyrinth

import akka.actor.{ActorRef, Props, ActorSystem}
import akka.testkit.{ImplicitSender, TestKit}
import common.MessageProtocol
import org.geneticmachine.db.Neo4JActor
import org.geneticmachine.navigation.algorithm.DijkstraBrain
import org.geneticmachine.machine.Guide
import org.scalatest._
import org.geneticmachine.navigation._
import test._
import scala.concurrent.Await
import scala.concurrent.duration._
import scala.util.Success
import akka.pattern.ask


class GuideTest(system: ActorSystem) extends TestKit(system) with FlatSpecLike with BeforeAndAfterAll with Matchers with ImplicitSender {
  def this() {
    this(ActorSystem("test"))
  }

  implicit val dbActor: ActorRef = system.actorOf(Props(new Neo4JActor("./genetic-machine-db")), "Neo4J")

  override def beforeAll() {
    cleanDirectory("./genetic-machine-db")
  }

  override def afterAll() {
    system.shutdown()
  }

  behavior of "Guard actor"

  it must "guide brain-robot" in {
    val brain = system.actorOf(Props(new DijkstraBrain(DijkstraBrain.empty)), "TheBrain")
    val robotFactory = LabyrinthRobot.sampleFactory
    val guide = system.actorOf(Props(new Guide(brain, robotFactory, 10.seconds)), "TheGuide")
    val result = Await.result(guide.ask(MessageProtocol.Init)(11.seconds), 11.seconds)

    result match {
      case Guide.GuideResult(Success(`brain`), Success(score), Success(serialization)) =>
        println(score)
        println(serialization)

      case msg =>
        println(msg)
        throw new Exception("Guide must answer with GuideResult")
    }
  }
}
