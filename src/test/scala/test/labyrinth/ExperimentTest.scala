package test.labyrinth

import akka.actor.{ActorRef, ActorSystem, Actor, Props}
import akka.testkit.TestKit

import geneticmachine.dataflow.DataFlowFormat
import geneticmachine.db.Neo4JActor
import geneticmachine.labyrinth.feedback.ZeroFeedback
import org.scalatest.WordSpecLike
import org.scalatest.Matchers
import org.scalatest.BeforeAndAfterAll

import scala.concurrent.duration._

import geneticmachine._
import geneticmachine.labyrinth._
import geneticmachine.labyrinth.generators.RandomWalkGenerator
import geneticmachine.labyrinth.vision.SimpleVision

import test._

class ExperimentTest (_system: ActorSystem) extends TestKit(_system)
with WordSpecLike with Matchers with BeforeAndAfterAll {

  def this() = this (ActorSystem("ExperimentTest"))

  override def afterAll() {
    //cleanDirectory("./test-db-driver")
    TestKit.shutdownActorSystem(_system)
  }

  class Guard extends Actor {
    def receive: Receive = {
      case props: Props =>
        val actor = context.actorOf(props)
        context.sender() ! actor

      case msg =>
        testActor forward msg
    }
  }

  "Experiment" should {
    "work" in {
      val labGen = RandomWalkGenerator(3, 5)(Point(71, 71))
      val vision = new SimpleVision(5)
      val feedback = ZeroFeedback

      val brainGen = (dff: Option[DataFlowFormat]) => {
        Props(classOf[DijkstraBrain], dff.getOrElse(DijkstraBrain.serialization()))
      }

      val robotGen = (brainRef: ActorRef) => {
        Props(classOf[LabyrinthRobot], brainRef, labGen, vision, feedback)
      }

      implicit val dbActor = _system.actorOf(Props(new Neo4JActor("./test-db-driver")))

      val guard = _system.actorOf(Props(new Guard))
      guard ! Props(new Experiment(None, brainGen, robotGen))

      expectMsgPF(max = 10.second, "Timeout") {
        case Experiment.Finish(bId, rId) =>
          guard ! Props(new Experiment(Some(bId), brainGen, robotGen))
        case msg =>
          throw new Exception(s"Bad receive: $msg")
      }

      expectMsgPF(max = 10.second, "Timeout") {
        case Experiment.Finish(bId, rId) =>
        case msg =>
          throw new Exception(s"Bad receive: $msg")
      }
    }
  }
}
