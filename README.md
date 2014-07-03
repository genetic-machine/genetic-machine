# Genetic + Machine

**GeneticMachine** is a tool for making experiments on AI algorithms. It's based on simple abstract architecture and focused at learning algorithms.

Although, the current development goal is to implement and solve so called 'labyrinth' task - an abstract long-distance navigation task.

**Version 0.1 has been released!**

## In a few words
```scala
import geneticmachine._
import geneticmachine.labyrinth.{LabyrinthRobot, DijkstraBrain, LabyrinthStatus}
import geneticmachine.machine.GeneticMachine
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.Success

// create local server
new GeneticMachine("GeneticMachine")

// connect to server through tcp
remote("GeneticMachine@127.0.0.1:7779") {
  using(DijkstraBrain).startWithNew.
    testWith(LabyrinthRobot.sampleFactory).repeat(3)
}.onComplete {
  case Success(results: Experiment.Result[LabyrinthStatus]) =>
    println(results.mkString("\n"))

  case other =>
    println(other)
}
```

## External libraries
*  [Scala](http://www.scala-lang.org/)
*  [Akka](http://akka.io/)
*  [Breeze](https://github.com/scalanlp/breeze)
*  [Neo4j Graph DB](http://www.neo4j.org/)
*  [Scala pickling](https://github.com/scala/pickling)

## Features and TODOs

### General

- [x] Basic concurrent Robot-Brain architecture based on actor model;
- [x] Fault-tolerance;
- [x] Generalized graph-oriented (dataflow-oriented) serialization format (Data Flow Format);

- [x] DB drivers:
  - [x] Neo4j driver;
  - [x] Pickle driver;

- [x] DB serialization:
  - [x] data flow;
  - [x] robot;
  - [x] brain;
  - [x] results.

- [ ] Visualization:
  - [x] through standard neo4j db server;
  - [ ] brain genealogy;
  - [ ] brain Data Flow;
  - [ ] custom robot visualization.

- [ ] Brains:
  - [x] Sample Dijkstra brain;
  - [ ] Decision fusion (one-layer network);
  - [ ] Idiotypic network (multi-layer network);
  - [ ] Idiotypic network mod. 2;
  - [ ] Adaptive dataflow;
  - [ ] Multi-brain shell.

- [x] Robots:
  - [x] Labyrinth Robot;
  - [ ] Multi-robot shell;

- [ ] Experiment functionality:
  - [x] basic experiment functionality;
  - [x] fault-tolerance;
  - [x] remote experiment;
  - [ ] experiment composition.

- [ ] General features:
  - [x] remote control (e.g. for work through scala console);
  - [ ] remote receptionist (for web-interface).

- [ ] General 'theoretical' features:
  - [x] feedback loop (for reinforcement learning);
  - [ ] unacceptable behaviour detection.

### Labyrinth specific
- [ ] labyrinth visualization;

- [ ] Labyrinth feedback:
  - [ ] a primitive feedback;
  - [ ] oracle-based (aka Laplace's demon);
  - [ ] based on brain's knowledge.

- [ ] Labyrinth generators:
  - [x] Random walk generator;
  - [ ] Office generator;
  - [ ] Maze-like generator.

- [ ] Vision:
  - [x] absolute (i.e. full vision);
  - [x] cost-map based (i.e. an approximation for true vision);
  - [x] BFS based (something in the middle of full and true visions);
  - [ ] ray/shadow cast (i.e. true vision).

- [ ] Efficiency metrics.
