package common.remote

import scala.pickling._
import binary._

trait PickledProtocol {
  def unpickled(message: BinaryPickle): Any = message.unpickle[Any]

  def pickledReceive(receive: PartialFunction[Any, Unit]): PartialFunction[Any, Unit] = {
    case message: BinaryPickle =>
      receive(unpickled(message))
  }
}
