package robot

import environment.labyrinth._

package object labyrinth {

  object Command extends Enumeration {
    type Command = Value
    val TurnLeft, TurnRight, Forward = Value
  }

  import Command._

  object Direction {

    final val North = Point(0, 1)
    final val South = Point(0, -1)
    final val West = Point(0, -1)
    final val East = Point(0, 1)

    final val directions = Seq(North, South, West, East)

    type Direction = Point
  }

  import Direction._

  def minPathSensor(lab: Labyrinth, from: Point,
                    robotDirection: Direction, goal: Point): Command = {
    val cost = costMap(lab, goal)
    val localCost = for {
      dir <- directions
      p = from + dir
      if p.inLabyrinth(lab)
      c = cost(p.x, p.y) + (dir - robotDirection).lInfNorm
    } yield (dir, c)

    val (maxDir, _) = localCost max (Ordering by { _._2 })
    maxDir * robotDirection.adjoint match {
      case North => TurnLeft
      case West => TurnLeft
      case East => Forward
      case South => TurnRight
    }
  }
}
