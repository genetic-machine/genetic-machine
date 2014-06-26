package geneticmachine.db.drivers

import geneticmachine.ubf._
import geneticmachine.ubf.UnifiedBrainFormat._

import java.io._

import scala.pickling._
import binary._

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

class PickleDriver(val dbPath: String) extends DBDriver {

  import PickleDriver._

  val filePattern = """brain_(\d+)\.bin""".r
  def getFileName(id: Long): String = s"brain_$id.bin"

  val dbDir = new File(dbPath)
  dbDir.mkdirs()

  var lastIndex: Long = {
    val files = dbDir.listFiles()

    val ids = for {
      file <- files
      name = file.getName
      anchors = filePattern.unapplySeq(name)
      if anchors.isDefined
    } yield anchors.get.apply(0).toLong

    if (ids.size > 0) ids.max + 1 else 0
  }

  override def saveBrain(ubf: UnifiedBrainFormat): Long = {
    val brainID = this.synchronized {
      val index = lastIndex
      lastIndex += 1
      index
    }

    val pickled = PickleUBF(ubf).pickle
    val file = new File(dbDir, getFileName(brainID))
    val fw = new FileOutputStream(file)
    fw.write(pickled.value)
    fw.close()

    brainID
  }

  override def loadBrain(brainId: Long): UnifiedBrainFormat = {
    val file = new File(dbDir, getFileName(brainId))
    val fr = new FileInputStream(file)
    val pickled = new Array[Byte](fr.available())
    fr.read(pickled)
    val PickleUBF(ubf) = BinaryPickle(pickled).unpickle[PickleUBF]

    ubf
  }

  override def shutdown(): Unit = ()
}
