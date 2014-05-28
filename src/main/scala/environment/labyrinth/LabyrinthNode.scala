package environment.labyrinth

import environment.LocationNode

class LabyrinthNode(location: Labyrinth)
  extends LocationNode[Labyrinth, Point,
                       Labyrinth, (Point, Point)](location) {

  override def observation(from: (Point, Point)): Labyrinth = {
    val (center, deep) = from
    location((center.x - deep.x) to (center.x + deep.x),
             (center.y - deep.y) to (center.y + deep.y))
  }

  override def locationParams = Point(location.rows, location.cols)
}
