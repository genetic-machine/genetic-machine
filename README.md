# Genetic + Machine

**GeneticMachine** is a tool for making experiments on AI algorithms. It's based on simple abstract architecture and oriented on learning algorithms.

Although, the current development goal is to implement and solve so called 'labyrinth' task - an abstract long-distance navigation task.

**Version 0.1 has been released!**

## In an few words
```scala
import geneticmachine._
import geneticmachine.labyrinth.{LabyrinthRobot, DijkstraBrain, LabyrinthStatus}

val result1: Future[Experiment.Result[LabyrinthStatus]] = GeneticMachine {
  using(DijkstraBrain).startWithNew.
    testWith(LabyrinthRobot.sampleFactory).repeat(2)
}

/** OR **/

val result2: Future[Experiment.Result[LabyrinthStatus]] =
  using(DijkstraBrain).startWith(28).
    testWith(LabyrinthRobot.sampleFactory).
      repeat(2).
        execute()
```

## Libraries
*  [Scala](http://www.scala-lang.org/)
*  [Akka](http://akka.io/)
*  [Breeze](https://github.com/scalanlp/breeze)
*  [Neo4j Graph DB](http://www.neo4j.org/)

## Features and TODOs

### General

- [x] Basic concurrent Robot-Brain architecture based on actor model (v0.1);
- [x] Fault-tolerance (v0.1);
- [x] Generalized graph-oriented (dataflow-oriented) serialization format (Data Flow Format) (v0.1);

- [x] DB serialization (v0.1):
  - [x] data flow (v0.1);
  - [x] robot (v0.1);
  - [x] brain (v0.1);
  - [x] results (v0.1).

- [ ] Visualization:
  - [x] through standard neo4j db server;
  - [ ] brain genealogy;
  - [ ] brain Data Flow;
  - [ ] custom robot visualization.

- [ ] Brains:
  - [x] Sample Dijkstra brain (v0.1);
  - [ ] Decision fusion (one-layer network);
  - [ ] Idiotypic network (multi-layer network);
  - [ ] Idiotypic network mod. 2;
  - [ ] Adaptive dataflow;
  - [ ] Multi-brain shell.

- [x] Robots:
  - [x] Labyrinth Robot (v0.1);
  - [ ] Multi-robot shell;

- [ ] Experiment functionality:
  - [x] basic experiment functionality (v0.1);
  - [x] fault-tolerance (v0.1);
  - [ ] remote experiment;
  - [ ] experiment composition.

- [ ] General features:
  - [ ] remote control (e.g. for work through scala console);
  - [ ] remote receptionist (for web-interface).

- [ ] General 'theoretical' features:
  - [x] Feedback loop (for reinforcement learning) (v0.1);
  - [ ] Unacceptable behaviour detection.

### Labyrinth specific
- [ ] labyrinth visualization;

- [ ] Labyrinth feedback:
  - [ ] a primitive feedback;
  - [ ] Oracle-based (aka Laplace's demon);
  - [ ] Based on brain's knowledge.

- [ ] Labyrinth generators:
  - [x] Random walk generator (v0.1);
  - [ ] Office generator;
  - [ ] Maze-like generator.

- [ ] Vision:
  - [x] absolute (v0.1);
  - [x] cost-map based (v0.1);
  - [x] BFS based (v0.1);
  - [ ] ray/shadow cast (i.e. true vision).

- [ ] Efficiency metrics.
