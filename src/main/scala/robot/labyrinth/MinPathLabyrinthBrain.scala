package robot.labyrinth

import robot.Brain
import environment.labyrinth.Labyrinth
import robot.labyrinth.Command.Command
import akka.actor.ActorRef

class MinPathLabyrinthBrain extends
  Brain[(Labyrinth, Command), Command] {

  import Brain._

  def doIntelligence(sensor: ActorRef, actuator: ActorRef): Receive = {
    case Input((_, command)) =>
      actuator ! Output(command)
    case Reset =>
      context.become(waitSensor)
      sender ! Done
  }
}
