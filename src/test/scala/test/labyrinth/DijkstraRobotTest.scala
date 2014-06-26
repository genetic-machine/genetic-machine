package test.labyrinth

import akka.actor.{ActorRef, ActorSystem, Actor, Props}
import akka.testkit.TestKit
import akka.pattern.ask

import common.MessageProtocol
import org.scalatest.WordSpecLike
import org.scalatest.Matchers
import org.scalatest.BeforeAndAfterAll

import scala.concurrent.duration._

import geneticmachine.labyrinth._
import geneticmachine.labyrinth.generators.LabyrinthGenerator
import geneticmachine.labyrinth.generators.RandomWalkGenerator
import geneticmachine.labyrinth.vision.{Vision, SimpleVision}
import geneticmachine.Robot

import scala.util.Success

class DijkstraRobotTest (_system: ActorSystem) extends TestKit(_system)
  with WordSpecLike with Matchers with BeforeAndAfterAll {

  def this() = this (ActorSystem("DijkstraTest"))

  override def afterAll() {
    TestKit.shutdownActorSystem(_system)
  }

  class RobotBrainActor(val labGen: LabyrinthGenerator, val vision: Vision) extends Actor {
    val brain = context.actorOf(Props(new DijkstraBrain()))
    val robot = context.actorOf(Props(new LabyrinthRobot(brain, labGen, vision)))

    override def receive: Receive = wait(robot)

//    def waitBrainInit: Receive = {
//      case MessageProtocol.Ready if context.sender() == brain =>
//        context.become(waitRobotInit(robot))
//
//      case msg =>
//        throw new Exception(s"Failed brain initialization! $msg")
//    }
//
//    def waitRobotInit(robot: ActorRef): Receive = {
//      case MessageProtocol.Ready if context.sender() == robot =>
//        context.become(wait(robot))
//
//      case msg =>
//        throw new Exception(s"Failed robot initialization! $msg")
//    }

    def wait(robot: ActorRef): Receive = {
      case msg if sender() == robot =>
        testActor forward msg
    }
  }

  "Dijkstra brain" should {
    "complete labyrinth" in {
      val labGen = RandomWalkGenerator(3, 5)(71, 71)
      val vision = new SimpleVision(5)

      _system.actorOf(Props(new RobotBrainActor(labGen, vision)))

      expectMsgPF(max = 3.second, "Timeout") {
        case Robot.Finish(_, stats: LabyrinthStatus) =>
        case msg =>
          throw new Exception(s"Bad receive: $msg")
      }
    }

    "complete labyrinth with 1-cell vision" in {
      val labGen = RandomWalkGenerator(3, 5)(71, 71)
      val vision = new SimpleVision(1)

      _system.actorOf(Props(new RobotBrainActor(labGen, vision)))

      expectMsgPF(max = 3.second, "Timeout") {
        case Robot.Finish(_, stats: LabyrinthStatus) =>
        case msg =>
          throw new Exception(s"Bad receive: $msg")
      }
    }
  }
}
