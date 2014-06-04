package environment.labyrinth

import environment.LocationNode
import environment.labyrinth.FullVision.vision
import akka.actor.Props

object LabyrinthNode {
  val props: Labyrinth => Props = { lab =>
    Props(classOf[LabyrinthNode], lab)
  }
}

class LabyrinthNode(location: Labyrinth)
  extends LocationNode[Labyrinth, Point,
                       Labyrinth, (Point, Int)](location) {

  override def observation(from: (Point, Int)): Labyrinth = {
    val (p, deep) = from
    vision(location, p, deep)
  }

  override def locationParams = Point(location.rows, location.cols)
}
