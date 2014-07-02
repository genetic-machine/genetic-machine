# Genetic + Machine

Version 0.1 has been released!

## Workflow

```scala
import geneticmachine._
import geneticmachine.labyrinth.{LabyrinthRobot, DijkstraBrain, LabyrinthStatus}

val result1: Future[Experiment.Result[LabyrinthStatus]] = GeneticMachine {
  using(DijkstraBrain).startWith(28).testWith(LabyrinthRobot.sampleFactory).repeat(2)
}

/** OR **/

val result2: Future[Experiment.Result[LabyrinthStatus]] =
  using(DijkstraBrain).startWith(28).
    testWith(LabyrinthRobot.sampleFactory).
      repeat(2).
        execute()
```
