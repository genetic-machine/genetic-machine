package geneticmachine.machine

import akka.actor._
import akka.pattern.ask
import com.typesafe.config.ConfigFactory
import common.{ MessageProtocol, ViewProtocol }
import common.dataflow.DataFlowFormat
import geneticmachine.ExperimentActor.ExperimentResult
import geneticmachine._
import geneticmachine.db.Neo4JActor
import org.neo4j.cypher.internal.compiler.v1_9.ExecutionContext
import scala.concurrent.duration._

import scala.concurrent.Future

abstract class Machine(val systemName: String = "GeneticMachine",
                       val configPath: String = "genetic-machine.conf") extends ExperimentContext {
  val system = ActorSystem(systemName, ConfigFactory.load(configPath))

  def log(message: Any) = system.log.info(s"$message")
  def logError(error: Throwable, message: Any = "") = system.log.error(error, s"$message")

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

  override def executeUntypedExperiment(experiment: Experiment[_, _, _, _]): Future[ExperimentResult[_]] = {
    val experimentActor = system.actorOf(Props(new ExperimentActor(experiment, dbActor)))
    val experimentResult = experimentActor.ask(MessageProtocol.Init)(experimentTimeout)

    experimentResult.onComplete {
      case result =>
        system.log.debug(s"Finish experiment $result;")
        system.stop(experimentActor)
    }

    experimentResult.mapTo[ExperimentResult[_]]
  }
}

class MirrorMachine(val mirroringSystemUrl: String = "GeneticMachine@127.0.0.1:7778",
                    configPath: String = "mirror.conf")
  extends Machine(s"MirrorOf${mirroringSystemUrl.filter{ p => p.isLetter }}", configPath) {

  val receptionistSelection = system.actorSelection(s"akka.tcp://$mirroringSystemUrl/user/receptionist")

  val experimentTimeout = 1.hour
  val resolvingTimeout = 5.seconds

  override def executeUntypedExperiment(ex: Experiment[_, _, _, _]): Future[ExperimentResult[_]] = {
    val resolving = receptionistSelection.resolveOne(resolvingTimeout)
    resolving.onFailure {
      case e: Throwable =>
        system.log.error(s"\n>> The system $mirroringSystemUrl seems to don't accept any connections.")
        system.log.debug(s"\n Can't connect to system $mirroringSystemUrl. Cause: $e.")
    }

    for {
      receptionist <- resolving
      result <- receptionist.ask(ex)(experimentTimeout).mapTo[ExperimentResult[_]]
    } yield result
  }
}

trait View {

  val askTimeout = 6.seconds

  val maxTraverseNodes = 1024
  val maxTraverseDepth = 1024

  val relationTypes = Seq(DataFlowFormat.parentRelation, DataFlowFormat.experimentRelation)

  def traverse(id: Option[Long]): Future[DataFlowFormat]
  def dff(id: Long): Future[DataFlowFormat]
}

trait ViewClient extends MirrorMachine with View {
  import scala.pickling._
  import binary._

  def withRemote[T](action: (ActorRef) => Future[T]): Future[T] = {
    val viewSelection = system.actorSelection(s"akka.tcp://$mirroringSystemUrl/user/view")
    val resolving = viewSelection.resolveOne(resolvingTimeout)

    resolving.onFailure {
      case e: Throwable =>
        system.log.error(s"\n>> The system $mirroringSystemUrl seems to don't accept any connections.")
        system.log.debug(s"\n Can't connect to system $mirroringSystemUrl. Cause: $e.")
    }

    for {
      view <- resolving
      result <- action(view)
    } yield result
  }

  def traverse(id: Option[Long]): Future[DataFlowFormat] = withRemote { view =>
    val req = ViewProtocol.Traverse(id, maxTraverseNodes, maxTraverseDepth).pickle
    for {
      pickled <- view.ask(req)(askTimeout).mapTo[BinaryPickle]
      ViewProtocol.Traversed(dff) = pickled.unpickle[ViewProtocol.Traversed]
    } yield dff
  }

  def dff(id: Long): Future[DataFlowFormat] = withRemote { view =>
    val req = ViewProtocol.GetDFF(id).pickle
    for {
      pickled <- view.ask(req)(askTimeout).mapTo[BinaryPickle]
      ViewProtocol.DFF(dff) = pickled.unpickle[ViewProtocol.DFF]
    } yield dff
  }
}

trait DirectView extends MachineDB with View {
  import geneticmachine.db.DBActor

  def dff(id: Long) = {
    (for {
      DBActor.Loaded(dff) <- dbActor.ask(DBActor.Load(id))(askTimeout)
    } yield dff).recover {
      case e: Throwable =>
        DataFlowFormat.errorDff("Error", e)
    }
  }

  def traverse(id: Option[Long]) = {
    val req = DBActor.Traverse(id, maxTraverseNodes, maxTraverseDepth, relationTypes)
    (for {
      DBActor.Traversed(dff) <- dbActor.ask(req)(askTimeout)
    } yield dff).recover {
      case e: Throwable =>
        DataFlowFormat.errorDff("Error", e)
    }
  }
}