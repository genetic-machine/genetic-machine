package geneticmachine.ubf.drivers

import geneticmachine.ubf._
import geneticmachine.ubf.UnifiedBrainFormat._

import scala.concurrent.{ExecutionContext, Future}
import scala.pickling._
import json._
import java.io._
import scala.io.Source

object PickleDriver {

  object PickleUBF {
    def unapply(pubf: PickleUBF): Option[UnifiedBrainFormat] = {
      Some(UnifiedBrainFormat(pubf.brainType, pubf.parentId, pubf.nodes.toVector, pubf.inputNodeId, pubf.outputNodeId))
    }

    def apply(ubf: UnifiedBrainFormat) = new PickleUBF(ubf.brainType, ubf.parentID, ubf.nodes.toList, ubf.inputNodeId, ubf.outputNodeId)
  }

  /** Pickler doesn't work with Vector[Node], but do work with List */
  final class PickleUBF(val brainType: String, val parentId: Long, val nodes: List[Node], val inputNodeId: Int, val outputNodeId: Int) {
  }
}

class PickleDriver(dbPath: String)(implicit val context: ExecutionContext) extends UnifiedBrainFormatDriver(dbPath) {

  import PickleDriver._

  val filePattern = """brain_(\d+)\.json""".r
  def getFileName(id: Long): String = s"brain_$id.json"

  val dbDir = new File(dbPath)
  dbDir.mkdirs()

  var lastIndex: Long = {
    val files = dbDir.listFiles()
    val ids = for {
      file <- files
      name = file.getName
      filePattern(idString) <- name
      id = idString.toLong
    } yield id

    if (ids.size > 0) ids.max + 1 else 0
  }

  override def save(ubf: UnifiedBrainFormat): Future[Long] = Future {
    val brainID = lastIndex
    lastIndex += 1

    val pickled = PickleUBF(ubf).pickle
    val file = new File(dbDir, getFileName(brainID))
    val pw = new PrintWriter(file)
    pw.write(pickled.value)
    pw.close()

    brainID
  }

  override def load(brainId: Long): Future[UnifiedBrainFormat] = Future {
    val path = new File(dbDir, getFileName(brainId)).getPath
    val pickled = Source.fromFile(path).mkString
    val PickleUBF(ubf) = JSONPickle(pickled).unpickle[PickleUBF]

    ubf
  }
}
