package geneticmachine.db.drivers

import common.dataflow._
import common.dataflow.DataFlowFormat._

import java.io._

import scala.pickling._
import binary._

object PickleDriver {

  object PickleDFF {
    def unapply(flow: PickleDFF): Option[DataFlowFormat] = {
      Some(DataFlowFormat(flow.props, flow.relations, flow.nodes.toVector, flow.inputNodeId, flow.outputNodeId))
    }

    def apply(dff: DataFlowFormat) = new PickleDFF(dff.props, dff.relations, dff.nodes.toList, dff.inputNodeId, dff.outputNodeId)
  }

  /** Pickler doesn't work with Vector[Node], but do work with List */
  final class PickleDFF(val props: Map[String, Any], val relations: Map[String, Set[Long]], val nodes: List[Node], val inputNodeId: Int, val outputNodeId: Int) {
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

  override def save(flow: DataFlowFormat): Long = {
    val brainID = this.synchronized {
      val index = lastIndex
      lastIndex += 1
      index
    }

    val pickled = PickleDFF(flow).pickle
    val file = new File(dbDir, getFileName(brainID))
    val fw = new FileOutputStream(file)
    fw.write(pickled.value)
    fw.close()

    brainID
  }

  override def load(brainId: Long): DataFlowFormat = {
    val file = new File(dbDir, getFileName(brainId))
    val fr = new FileInputStream(file)
    val pickled = new Array[Byte](fr.available())
    fr.read(pickled)
    val PickleDFF(dff) = BinaryPickle(pickled).unpickle[PickleDFF]

    dff.idInjection(brainId)
  }

  override def shutdown(): Unit = ()
}
