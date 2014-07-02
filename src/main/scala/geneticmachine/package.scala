package object geneticmachine {
  implicit val geneticMachine: ExperimentContext = GeneticMachine

  def using[I, O, F](brainFactory: BrainFactory[I, O, F]) = Experiment.using[I, O, F](brainFactory)
}
