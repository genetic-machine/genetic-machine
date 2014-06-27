package geneticmachine.db.drivers

import geneticmachine.dataflow.DataFlowFormatBuilder.NodeRef
import geneticmachine.dataflow.{DataFlowFormat => DFF, DataFlowFormatBuilder}

import geneticmachine.dataflow.DataFlowFormat._
import geneticmachine.dataflow.DataFlowFormat.{ Node => DFFNode}

import org.neo4j.graphdb._
import org.neo4j.graphdb.factory.GraphDatabaseFactory
import org.neo4j.graphdb.{ Node => NeoNode }

import scala.util.{Success, Try, Failure}

import scala.collection.JavaConversions._

object Neo4JDriver {

  val connected = DynamicRelationshipType.withName("CONNECTED")
  val write = DynamicRelationshipType.withName("WRITE")
  val read = DynamicRelationshipType.withName("READ")

  val portFromProp = "$portFrom"
  val portToProp = "$portTo"
}

final class Neo4JDriver(val dbPath: String) extends DBDriver {

  import Neo4JDriver._

  val graphDB = new GraphDatabaseFactory().newEmbeddedDatabase(dbPath)

  val shutdownHook = scala.sys.addShutdownHook {
    graphDB.shutdown()
  }

  def shutdown() = { shutdownHook.remove(); graphDB.shutdown() }

  def createNode(props: Map[String, Any]): NeoNode = {
    val label = props.getOrElse(labelProp, nodeLabel).asInstanceOf[String]
    val neoNode = graphDB.createNode(DynamicLabel.label(label))
    for {
      (k, v) <- props
    } {
      neoNode.setProperty(k, v)
    }

    neoNode
  }

  def createNode(dffNode: DFFNode): NeoNode = {
    val props = dffNode.props.updated(inputsProp, dffNode.inputs).updated(outputsProp, dffNode.outputs)
    createNode(props)
  }

  def withTx[T](body: => T): T = {
    val tx = graphDB.beginTx()
    val result = Try {
      body
    }

    result match {
      case Success(_) =>
        tx.success()
      case Failure(_) =>
        tx.failure()
    }

    tx.close()

    result.get
  }

  override def save(dff: DFF): Long = withTx {
    val mainNode = createNode(dff.props)

    val dffIdToNeoNode = (for {
      (dffNode, dffId) <- dff.nodes.zipWithIndex
      neoNode = createNode(dffNode)
    } yield (dffId, neoNode)).toMap

    for {
      (dffNodeId, neoNode) <- dffIdToNeoNode
      (outs, port) <- dff.node(dffNodeId).edges.zipWithIndex
      Port(outNode, outPort) <- outs
    } {
      val outNeoNode = dffIdToNeoNode(outNode)
      val r = neoNode.createRelationshipTo(outNeoNode, connected)
      r.setProperty(portFromProp, port)
      r.setProperty(portToProp, outPort)
    }

    val inputNode = dffIdToNeoNode(dff.inputNodeId)
    val outputNode = dffIdToNeoNode(dff.outputNodeId)

    mainNode.createRelationshipTo(inputNode, write)
    mainNode.createRelationshipTo(outputNode, read)

    for {
      (relationType, id) <- dff.relations
    } {
      val endNode = graphDB.getNodeById(id)
      mainNode.createRelationshipTo(endNode, DynamicRelationshipType.withName(relationType))
    }

    mainNode.getId
  }

  private def getInputsOutputs(node: NeoNode): (Int, Int) = {
    val ins = node.getProperty(inputsProp).asInstanceOf[Int]
    val outs = node.getProperty(outputsProp).asInstanceOf[Int]
    (ins, outs)
  }

  private def getPorts(rel: Relationship): (Int, Int) = {
    val portFrom = rel.getProperty(portFromProp).asInstanceOf[Int]
    val portTo = rel.getProperty(portToProp).asInstanceOf[Int]
    (portFrom, portTo)
  }

  override def load(id: Long): DFF = withTx {
    val mainNode = graphDB.getNodeById(id)

    def getOnlyConnectedNode(node: NeoNode, rel: RelationshipType): NeoNode = {
      val rels = node.getRelationships(Direction.OUTGOING, rel).toList
      if (rels.length != 1) {
        throw new NotFoundException("No or multiple inputs!")
      }

      rels(0).getEndNode
    }

    def getNonReadWriteRelationships(node: NeoNode): Map[String, Long] = {
      (for {
        rel <- node.getRelationships(Direction.OUTGOING)
        if !rel.isType(write) && !rel.isType(read)
      } yield (rel.getType.name(), rel.getEndNode.getId)).toMap
    }

    val inputNode = getOnlyConnectedNode(mainNode, write)
    val outputNode = getOnlyConnectedNode(mainNode, read)

    def traverse(open: Set[NeoNode], closed: Set[NeoNode]): Set[NeoNode] = {
      val wave = for {
        node <- open
        rel <- node.getRelationships(Direction.OUTGOING, connected).toList
        otherNode = rel.getEndNode
        if !closed.contains(otherNode) && !open.contains(otherNode)
      } yield otherNode

      if (wave.isEmpty) {
        open ++ closed
      } else {
        traverse(wave, open ++ closed)
      }
    }

    val neoNodes = traverse(Set(inputNode), Set.empty)

    val dffBuilder = DataFlowFormatBuilder(mainNode.getLabels.toList(0).name())

    def createDFFNode(neoNode: NeoNode): NodeRef = {
      val (inputs, outputs) = getInputsOutputs(neoNode)
      val ref = dffBuilder.node(inputs, outputs)

      val propKeys = neoNode.getPropertyKeys.toSet -- Set(inputsProp, outputsProp)

      for {
        key <- propKeys
        value = neoNode.getProperty(key)
      } {
        ref(key -> value)
      }

      ref
    }

    val neoToDff = (for {
      neoNode <- neoNodes
      dffNode = createDFFNode(neoNode)
    } yield (neoNode, dffNode)).toMap

    for {
      (neoNode, startDffNode) <- neoToDff
      rel <- neoNode.getRelationships(Direction.OUTGOING, connected).toList
      endDffNode = neoToDff(rel.getEndNode)
      (portFrom, portTo) = getPorts(rel)
    } {
      startDffNode(portFrom) --> endDffNode(portTo)
    }

    dffBuilder.withInput(neoToDff(inputNode)).withOutput(neoToDff(outputNode)).toDataFlowFormat
  }
}
