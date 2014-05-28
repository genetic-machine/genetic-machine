package common

trait MessageProtocol {
  abstract class Request

  abstract class Response
  case object Done extends Response
  case object Fail extends Response
  case object Ready extends Response

  final class MissType(msg: String) extends Throwable(msg)

  object MissType {
    def apply[T](clazz: Class[T]) = new MissType(s"doesn't conform with ${clazz.toString}")
  }
}
