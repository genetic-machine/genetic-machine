package org.geneticmachine

import org.geneticmachine.machine.{SimpleMachine, SimpleExecutionContext, DBDriver}
import org.geneticmachine.machine.db.Neo4JDriver
import org.scalatest.{Matchers, FlatSpec}

import scala.util.{Success, Try}

class ExperimentTest extends FlatSpec with Matchers {
  behavior of "Experiment"

  it must "generate proper sequence" in {
    cleanDirectory("test-2-db")

    import Experiment._

    val experiment = {
      (IncAlgorithmGen -> IntEnvironmentGen).then {
        until { er: ExperimentResult[List[Int]] =>
          er.steps.size > 10
        }(IncAlgorithmGen -> IntEnvironmentGen)
      }
    }

    implicit val db: DBDriver = new Neo4JDriver("test-2-db")
    implicit val context: ExecutionContext = new SimpleExecutionContext

    val machine = new SimpleMachine

    println {
      machine {
        experiment
      }
    }

    db.shutdown()

    cleanDirectory("test-2-db")
  }
}
