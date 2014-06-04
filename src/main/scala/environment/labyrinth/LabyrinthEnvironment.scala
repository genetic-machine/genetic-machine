package environment.labyrinth

import environment.Environment

class LabyrinthEnvironment extends
  Environment[Point, Labyrinth] (LabyrinthNode.props) {

  override def generateLocation(params: Point) = {
    simpleLabyrinthGen(params.x, params.y)
  }
}
