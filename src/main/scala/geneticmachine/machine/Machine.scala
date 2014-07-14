package geneticmachine.machine

import akka.actor._
import akka.pattern.ask
import com.typesafe.config.ConfigFactory
import common.MessageProtocol
import geneticmachine._
import geneticmachine.db.{PickleActor, Neo4JActor}
import scala.concurrent.duration._

import scala.concurrent.Future

abstract class Machine(val systemName: String = "GeneticMachine",
                       val configPath: String = "genetic-machine.conf") extends ExperimentContext {
  val system = ActorSystem(systemName, ConfigFactory.load(configPath))
  implicit val dispatcher = system.dispatcher

  def shutdown() { system.shutdown(); system.awaitTermination() }
}

trait MachineDB extends Machine {
  val dbPath: String = "./genetic-machine-db"
  val dbActor: ActorRef
}

trait Neo4jDB extends MachineDB {
  val dbActor = system.actorOf(Props(new Neo4JActor(dbPath)), "DB")
}

trait PickleDB extends MachineDB {
  val dbActor = system.actorOf(Props(new PickleActor(dbPath)), "DB")
}

trait RemoteView extends MachineDB {
  val viewActor = system.actorOf(Props(new ViewActor(dbActor)), "view")
}

trait RemoteControl extends Machine {
  val receptionist = system.actorOf(Props(new Receptionist(this)), "receptionist")
}

abstract class GeneticMachine(systemName: String = "GeneticMachine",
                              configPath: String = "genetic-machine.conf",
                              override val dbPath: String = "./genetic-machine-db")
  extends Machine(systemName, configPath) with MachineDB {

  val experimentTimeout = 1.hour

  override def executeUntypedExperiment(experiment: Experiment[_, _, _, _]): Future[Any] = {
    val experimentActor = system.actorOf(Props(new ExperimentActor(experiment, dbActor)))
    val experimentResult = experimentActor.ask(MessageProtocol.Init)(experimentTimeout)

    experimentResult.onComplete {
      case _ =>
        system.stop(experimentActor)
    }

    for {
      ExperimentActor.ExperimentResult(result) <- experimentResult
    } yield result
  }
}

class MirrorMachine(val mirroringSystemUrl: String = "GeneticMachine@127.0.0.1:7779",
                    configPath: String = "mirror.conf")
  extends Machine(s"MirrorOf${mirroringSystemUrl.filter{ p => p.isLetter }}", configPath) {

  val receptionistSelection = system.actorSelection(s"akka.tcp://$mirroringSystemUrl/user/receptionist")

  val experimentTimeout = 1.hour

  override def executeUntypedExperiment(ex: Experiment[_, _, _, _]): Future[Any] = {
    val resolving = receptionistSelection.resolveOne(1.minute)
    resolving.onFailure {
      case e: Throwable =>
        system.log.error(s"\n>> The system $mirroringSystemUrl seems to don't accept any connections.")
        system.log.debug(s"\n Can't connect to system $mirroringSystemUrl. Cause: $e.")
    }

    for {
      receptionist <- resolving
      result <- receptionist.ask(ex)(experimentTimeout)
    } yield result
  }
}