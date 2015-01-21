package test.labyrinth

import akka.actor.{ActorRef, ActorSystem, Actor, Props}
import akka.testkit.TestKit
import akka.pattern.ask

import common.MessageProtocol
import org.geneticmachine.navigation.algorithm.DijkstraBrain
import org.geneticmachine.navigation.feedback.ZeroFeedback
import org.geneticmachine.machine.RobotResult
import org.scalatest.WordSpecLike
import org.scalatest.Matchers
import org.scalatest.BeforeAndAfterAll

import scala.concurrent.duration._

import org.geneticmachine.navigation._
import org.geneticmachine.navigation.generators.LabyrinthGenerator
import org.geneticmachine.navigation.generators.RandomWalkGenerator
import org.geneticmachine.navigation.vision.{Vision, SimpleVision}
import org.geneticmachine.RobotActor$

import scala.util.Success

class DijkstraRobotTest (_system: ActorSystem) extends TestKit(_system)
  with WordSpecLike with Matchers with BeforeAndAfterAll {

  def this() = this (ActorSystem("DijkstraTest"))

  override def afterAll() {
    TestKit.shutdownActorSystem(_system)
  }

  class RobotBrainActor(val labGen: LabyrinthGenerator, val vision: Vision) extends Actor {
    val brain = context.actorOf(Props(new DijkstraBrain(DijkstraBrain.empty)))
    val robot = context.actorOf(Props(new LabyrinthRobot(brain, labGen, vision, ZeroFeedback, Nil, Nil)))

    override def receive: Receive = wait(robot)

    def wait(robot: ActorRef): Receive = {
      case msg if sender() == robot =>
        testActor forward msg
    }
  }

  "Dijkstra brain" should {
    "complete labyrinth" in {
      val labGen = RandomWalkGenerator(3, 5)(Point(71, 71))
      val vision = new SimpleVision(5)

      _system.actorOf(Props(new RobotBrainActor(labGen, vision)))

      expectMsgPF(max = 10.second, "Timeout") {
        case RobotActor.Finish(_, stats: RobotResult[NavigationState]) =>
        case msg =>
          throw new Exception(s"Bad receive: $msg")
      }
    }

    "complete labyrinth with 1-cell vision" in {
      val labGen = RandomWalkGenerator(3, 5)(Point(71, 71))
      val vision = new SimpleVision(1)

      _system.actorOf(Props(new RobotBrainActor(labGen, vision)))

      expectMsgPF(max = 10.second, "Timeout") {
        case RobotActor.Finish(_, stats: RobotResult[NavigationState]) =>
        case msg =>
          throw new Exception(s"Bad receive: $msg")
      }
    }
  }
}
