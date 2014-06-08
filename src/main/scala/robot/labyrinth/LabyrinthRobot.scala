package robot.labyrinth

import robot.Robot
import Direction._
import Command._
import akka.actor.ActorRef
import scala.concurrent.Future
import breeze.linalg.DenseMatrix
import Vision._
import CellStatus._

case class LabyrinthStatus(visionMap: Labyrinth, labyrinth: Labyrinth,
                           robotPosition: Point, robotDirection: Direction,
                           goal: Point) {

  def printVisionMap: DenseMatrix[Char] = {
    val result = printLab(visionMap)
    result(robotPosition.x, robotPosition.y) = robotDirection match {
      case North => 'V'
      case South => 'A'
      case West => '>'
      case East => '<'
    }

    result
  }
}

class LabyrinthRobot(brain: ActorRef, val sizeX: Int, val sizeY: Int, val sensorDeep: Int)
  extends Robot[DijkstraInput, LabyrinthStatus, Command](brain) {

  import context.dispatcher

  def selfSetup() = Future {
    val lab = simpleLabyrinthGen(sizeX, sizeY)
    val visionMap = DenseMatrix.fill[Int](sizeX, sizeY)(Unknown)
    val initialP = Point(0, (sizeY - 1) / 2)
    val goal = Point(sizeX - 1, (sizeY - 1) / 2)
    applyVision(visionMap, vision(lab, initialP, sensorDeep), initialP)
    val status = LabyrinthStatus(visionMap, lab, initialP, North, goal)
    val initialInput = DijkstraInput(visionMap, initialP, North, goal)

    println(status.printVisionMap)
    (status, initialInput)
  }

  def processOutput(status: LabyrinthStatus, brainOutput: Command): Option[(LabyrinthStatus, DijkstraInput)] = {
    val (newPosition, newDirection) = brainOutput match {
      case Forward if (status.robotPosition + status.robotDirection).inLabyrinth(status.labyrinth) =>
        (status.robotPosition + status.robotDirection, status.robotDirection)
      case Forward =>
        (status.robotPosition, status.robotDirection)
      case TurnLeft =>
        (status.robotPosition, status.robotDirection.turnLeft)
      case TurnRight =>
        (status.robotPosition, status.robotDirection.turnRight)
    }


    val visionDiff = vision(status.labyrinth, newPosition, sensorDeep)
    applyVision(status.visionMap, visionDiff, newPosition)
    val newStatus = LabyrinthStatus(status.visionMap, status.labyrinth, newPosition, newDirection, status.goal)
    val newInput = DijkstraInput(status.visionMap, newPosition, newDirection, status.goal)
    println(newStatus.printVisionMap)

    if (newPosition == status.goal) {
      println("\nGOAL!!!\n")
      None
    } else {
      Some((newStatus, newInput))
    }
  }

  def training(status: LabyrinthStatus) = guideBrain(status)
}
