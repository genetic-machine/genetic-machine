package geneticmachine

import geneticmachine.labyrinth.{LabyrinthRobot, DijkstraBrain}

import scala.collection.immutable.Queue
import scala.concurrent.Future
import scala.reflect.ClassTag

object Experiment {

  final case class Using[I, O, F](brainFactory: BrainFactory[I, O, F]) {
    def startWith(id: Long) = new EmptyExperiment(brainFactory, Some(id))
    def startWithNew = new EmptyExperiment(brainFactory, None)
  }

  def using[I, O, F](brainFactory: BrainFactory[I, O, F]) = Using[I, O, F](brainFactory)
}

trait ExperimentContext {
  def executeExperiment[S : ClassTag](experiment: Experiment[_, _, _, S]): Future[List[S]]
}

sealed abstract class Experiment[I, O, F, +S : ClassTag]
  (val brainFactory: BrainFactory[I, O, F],
   val startWith: Option[Long],
   val cycles: Queue[RobotFactory[I, O, F, S]]) {

  def testWith[T >: S](robotFactory: RobotFactory[I, O, F, T])(implicit ev: ClassTag[T]): NonEmptyExperiment[I, O, F, T] = {
    new NonEmptyExperiment(brainFactory, startWith, cycles.enqueue(robotFactory))
  }

  def execute()(implicit context: ExperimentContext): Future[List[S]] = {
    context.executeExperiment(this)
  }
}

final class EmptyExperiment[I, O, F](brainFactory: BrainFactory[I, O, F],
                                     startWith: Option[Long])
  extends Experiment[I, O, F, Nothing](brainFactory, startWith, Queue.empty)

final class NonEmptyExperiment[I, O, F, +S : ClassTag]
  (brainFactory: BrainFactory[I, O, F],
   startWith: Option[Long],
   cycles: Queue[RobotFactory[I, O, F, S]])
  extends Experiment[I, O, F, S](brainFactory, startWith, cycles) {

  def repeat(count: Int): NonEmptyExperiment[I, O, F, S] = {
    val last = cycles.last
    val newCycles = (0 until count).foldLeft(cycles) { (acc, _) =>
      acc.enqueue(last)
    }

    new NonEmptyExperiment(brainFactory, startWith, newCycles)
  }
}