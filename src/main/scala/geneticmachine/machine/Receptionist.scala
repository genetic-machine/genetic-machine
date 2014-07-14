package geneticmachine.machine

import akka.actor.{ActorLogging, Actor}
import akka.pattern.pipe
import geneticmachine.{ExperimentContext, Experiment}

class Receptionist(val experimentContext: ExperimentContext) extends Actor with ActorLogging {
  import context.dispatcher

  override def receive: Receive = {
    case ex: Experiment[_, _, _, _] =>
      experimentContext.executeUntypedExperiment(ex).pipeTo(context.sender())
  }
}
