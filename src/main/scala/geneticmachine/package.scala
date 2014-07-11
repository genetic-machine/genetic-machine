import geneticmachine.machine._

package object geneticmachine {
  def remote: Machine = new MirrorMachine()
  def remote(where: String): Machine = new MirrorMachine(where)

  def using[I, O, F](brainFactory: BrainFactory[I, O, F]) = Experiment.using[I, O, F](brainFactory)
}
