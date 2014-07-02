package geneticmachine

import akka.actor.{Props, ActorSystem}
import akka.pattern.ask
import common.MessageProtocol
import geneticmachine.Experiment.Result
import geneticmachine.db.Neo4JActor

import scala.concurrent._
import duration._
import scala.reflect.ClassTag

object GeneticMachine extends ExperimentContext {
  val system = ActorSystem("Genetic-Machine")
  val dbPath = "./test-db-driver"
  val dbActor = system.actorOf(Props(new Neo4JActor(dbPath)))
  val experimentTimeout = 1.hour

  override def executeExperiment[S : ClassTag](experiment: Experiment[_, _, _, S]): Future[Result[S]] = {
    val experimentActor = system.actorOf(Props(new ExperimentActor[S](experiment, dbActor)))
    experimentActor.ask(MessageProtocol.Init)(experimentTimeout).mapTo[Result[S]]
  }

  def shutdown() {
    system.shutdown()
  }
}
