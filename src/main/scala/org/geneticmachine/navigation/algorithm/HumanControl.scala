//package org.geneticmachine.navigation.algorithm
//
//import org.geneticmachine.navigation._
//
//import scala.concurrent.Future
//
//object HumanControl extends LabyrinthBrainFactory {
//  override def props(dff: DataFlowFormat) = Props { new HumanControl(dff) }
//}
//
//class HumanControl(dff: DataFlowFormat) extends LabyrinthBrain[Integer](dff) {
//  import context.dispatcher
//
//  override def init: Future[Integer] = Future { 0: Integer }
//
//  private val commandMap = Map[Char, NavigationCommand] (
//    'a' -> NavigationCommand.TurnLeft,
//    'w' -> NavigationCommand.Forward,
//    'd' -> NavigationCommand.TurnRight
//  )
//
//  private def getCommand(): NavigationCommand = {
//    val c = readChar().toLower
//    if (commandMap.contains(c)) {
//      commandMap(c)
//    } else {
//      getCommand()
//    }
//  }
//
//  override def input(state: Integer, inputData: NavigationInput): Future[(Integer, NavigationCommand)] = Future {
//    val obs = inputData.observation.orientated
//    val cm = labToCharMatrix(obs.visionMap)
//    val rp = obs.from
//    cm(rp.point.x, rp.point.y) = Direction.char(rp.direction)
//    log.info(s"Input on $state")
//    log.info(s"\n${ charMatrixToString(cm) }")
//
//    (state + 1: Integer, getCommand())
//  }
//
//  override def serialize(state: Integer) = Future.successful {
//    log.info(s"Reset on $state")
//    HumanControl.empty
//  }
//
//  override def feedback(state: Integer, score: LabyrinthFeedback) = Future {
//    log.info(s"Feedback ${score.value} on $state")
//    state
//  }
//
//  override def reset(state: Integer) = Future {
//    log.info(s"Reset on $state")
//    state
//  }
//}
