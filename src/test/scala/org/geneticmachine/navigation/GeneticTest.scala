package org.geneticmachine.navigation

import org.geneticmachine._
import navigation._
import navigation.vision._

import algorithm.{FusionMutator, MutatorParams}
import org.geneticmachine.navigation.utils.NavigationInfo

import org.scalatest.{Matchers, FlatSpec}

class GeneticTest extends FlatSpec with Matchers {
  "Mutator" must "mutate" in {
    val params = new MutatorParams {
      val additionalCoefsLen: Int = 2

      override val patternR: Int = 3
      override val patternRD: Int = 1

      override val learningSpeed: Double = 0.01
      override val similarityCoefs: Double = 0.1

      override val defaultStrength: Double = 0.1

      override val generationPatternD: Double = 0.5
      override val generationCoefsD: Double = 0.5

      override val adaptationPatternD: Double = 0.05
      override val adaptationCoefsD: Double = 0.05
    }

    val labStr = Seq(
     //0123456
      "#######", //0
      "#  ####", //1
      "#  ####", //2
      "#     #", //3
      "##### #", //4
      "#     #", //5
      "# #####", //6
      "#     #", //7
      "####  #", //8
      "#######"  //9
    )

    val lab = labFromChars(labStr)

    def printDir(dir: Direction): Unit = {
      val vo = VisionObservation(lab, RobotPosition(Point(2, 1), dir))

      println()

      println {
        NavigationInfo.horzConcat("origin", "orientated")(s"$vo", s"${vo.orientated}")
      }

      println()
    }

    printDir(Direction.North)
    printDir(Direction.West)
    printDir(Direction.South)
    printDir(Direction.East)

    //val mutator = FusionMutator(params)
  }
}
