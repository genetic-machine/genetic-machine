package test.labyrinth

import akka.actor.ActorSystem
import akka.actor.Actor
import akka.actor.Props
import akka.testkit.TestKit
import com.typesafe.config._
import org.scalatest.WordSpecLike
import org.scalatest.Matchers
import org.scalatest.BeforeAndAfterAll

import robot.labyrinth._
import robot.labyrinth.generators.RandomWalkGenerator
import robot.labyrinth.vision.SimpleVision
import robot.Robot

class DijkstraRobotTest (_system: ActorSystem) extends TestKit(_system)
  with WordSpecLike with Matchers with BeforeAndAfterAll {

  def this() = this (ActorSystem("DijkstraTest"))

  override def afterAll() {
    TestKit.shutdownActorSystem(_system)
  }

  "Dijkstra brain" should {
    "complete labyrinth" in {
      val brain = _system.actorOf(Props(new DijkstraBrain()))
      val labGen = RandomWalkGenerator(3, 5)(31, 31)
      val vision = new SimpleVision(5)

      _system.actorOf(Props(new Actor {
        val robot = context.actorOf(Props(new LabyrinthRobot(brain, labGen, vision)))

        override def receive: Receive = {
          case msg =>
            testActor forward msg
        }
      }))

      expectMsgClass(classOf[Robot.Finish[LabyrinthStatus]])
    }

    "complete labyrinth with 1-cell vision" in {
      val brain = _system.actorOf(Props(new DijkstraBrain()))
      val labGen = RandomWalkGenerator(3, 5)(31, 31)
      val vision = new SimpleVision(1)

      _system.actorOf(Props(new Actor {
        val robot = context.actorOf(Props(new LabyrinthRobot(brain, labGen, vision)))

        override def receive: Receive = {
          case msg =>
            testActor forward msg
        }
      }))

      expectMsgClass(classOf[Robot.Finish[LabyrinthStatus]])
    }
  }
}
