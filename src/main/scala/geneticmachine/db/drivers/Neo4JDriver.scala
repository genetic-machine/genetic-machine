package geneticmachine.db.drivers

import java.util.concurrent.TimeUnit

import common.dataflow.DataFlowFormatBuilder.NodeRef
import common.dataflow.{DataFlowFormat => DFF, DataFlowFormatBuilder}

import common.dataflow.DataFlowFormat._
import common.dataflow.DataFlowFormat.{ Node => DFFNode}

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

  /** This prop is only for fast search of genealogy roots. **/
  val nullParentId: Long = -1l
  val parentProp = "$parentId"
}

final class Neo4JDriver(val dbPath: String) extends DBDriver {

  import Neo4JDriver._

  val graphDB = new GraphDatabaseFactory().newEmbeddedDatabase(dbPath)

  /** Initialize or create brain index **/
  withTx {
    val brainDlabel = DynamicLabel.label(brainLabel)
    val brainIndexes = graphDB.schema().getIndexes(brainDlabel)

    if (!brainIndexes.exists { ind => ind.getPropertyKeys.toList.contains(parentProp) }) {
      graphDB.schema().indexFor(brainDlabel).on(parentProp).create()
    }
  }

  withTx {
    graphDB.schema().awaitIndexesOnline(1, TimeUnit.MINUTES)
  }

  def shutdown() = { graphDB.shutdown() }

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
    val mainNode = createNode {
      val parentId = if (dff.relations.contains(parentRelation)) {
        dff.relations(parentRelation).head
      } else {
        nullParentId
      }

      dff.props + (parentProp -> parentId)
    }

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
      (relationType, ids) <- dff.relations
      id <- ids
    } {
      Try {
        val endNode = graphDB.getNodeById(id)
        mainNode.createRelationshipTo(endNode, DynamicRelationshipType.withName(relationType))
      }
    }

    mainNode.getId
  }

  def getInputsOutputs(node: NeoNode): (Int, Int) = {
    val ins = node.getProperty(inputsProp).asInstanceOf[Int]
    val outs = node.getProperty(outputsProp).asInstanceOf[Int]
    (ins, outs)
  }

  def getPorts(rel: Relationship): (Int, Int) = {
    val portFrom = rel.getProperty(portFromProp).asInstanceOf[Int]
    val portTo = rel.getProperty(portToProp).asInstanceOf[Int]
    (portFrom, portTo)
  }

  def bfs(open: Set[NeoNode], closed: Set[NeoNode]): Set[NeoNode] = {
    val wave = for {
      node <- open
      rel <- node.getRelationships(Direction.OUTGOING, connected).toList
      otherNode = rel.getEndNode
      if !closed.contains(otherNode) && !open.contains(otherNode)
    } yield otherNode

    if (wave.isEmpty) {
      open ++ closed
    } else {
      bfs(wave, open ++ closed)
    }
  }

  def createMainDffNode(neoNode: NeoNode, dffBuilder: DataFlowFormatBuilder): NodeRef = {
    val ref = dffBuilder.node()

    for {
      prop <- neoNode.getPropertyKeys
      value = neoNode.getProperty(prop)
    } {
      ref(prop -> value)
    }

    ref
  }

  def createDFFNode(neoNode: NeoNode, dffBuilder: DataFlowFormatBuilder): NodeRef = {
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

  override def load(id: Long): DFF = withTx {
    val mainNode = graphDB.getNodeById(id)

    def getOnlyConnectedNode(node: NeoNode, rel: RelationshipType): NeoNode = {
      val relations = node.getRelationships(Direction.OUTGOING, rel).toList
      if (relations.length != 1) {
        throw new NotFoundException("No or multiple inputs!")
      }

      relations(0).getEndNode
    }

    val inputNode = getOnlyConnectedNode(mainNode, write)
    val outputNode = getOnlyConnectedNode(mainNode, read)

    val neoNodes = bfs(Set(inputNode), Set.empty)

    val dffBuilder = DataFlowFormatBuilder(mainNode.getLabels.toList(0).name())

    for {
      prop <- mainNode.getPropertyKeys.toSet -- Set(parentProp)
      value = mainNode.getProperty(prop)
    } {
      dffBuilder(prop -> value)
    }

    val neoToDff = (for {
      neoNode <- neoNodes
      dffNode = createDFFNode(neoNode, dffBuilder)
    } yield (neoNode, dffNode)).toMap

    for {
      (neoNode, startDffNode) <- neoToDff
      rel <- neoNode.getRelationships(Direction.OUTGOING, connected).toList
      endDffNode = neoToDff(rel.getEndNode)
      (portFrom, portTo) = getPorts(rel)
    } {
      startDffNode(portFrom) --> endDffNode(portTo)
    }

    /** ID injection **/
    dffBuilder.withId(id)

    dffBuilder.withInput(neoToDff(inputNode)).withOutput(neoToDff(outputNode)).toDataFlowFormat
  }

  def getStartNodes(startNodeId: Option[Long], limit: Int): Set[NeoNode] = {
    if (startNodeId.isEmpty) {
      graphDB.findNodesByLabelAndProperty(DynamicLabel.label(brainLabel), parentProp,
        nullParentId).take(limit).toSet
    } else {
      Set(graphDB.getNodeById(startNodeId.get))
    }
  }

  def bidirectionalBfs(startNodes: Set[NeoNode], depth: Int,
                       limit: Long, permittedConnections: Seq[String]): Set[NeoNode] = {
    val permittedRelations = permittedConnections.map { c => DynamicRelationshipType.withName(c) }.toArray

    def bfs(open: Set[NeoNode], closed: Set[NeoNode], depth: Int, limit: Long): Set[NeoNode] = {
      if (limit <= 0 || depth <= 0) {
        open ++ closed
      } else {
        val wave = for {
          node <- open
          rel <- node.getRelationships(Direction.BOTH, permittedRelations: _*)
          neigh = rel.getOtherNode(node)
          if !(open.contains(neigh) || closed.contains(neigh))
        } yield neigh

        if (wave.isEmpty) {
          open ++ closed
        } else {
          bfs(wave, open ++ closed, depth - 1, limit - wave.size)
        }
      }
    }

    bfs(startNodes, Set.empty, depth, limit - startNodes.size)
  }

  override def traverse(startNode: Option[Long], depth: Int,
                        limit: Long, permittedConnections: Seq[String]): DFF = withTx[DFF] {
    val startNodes = getStartNodes(startNode, limit.toInt)

    val neoNodes = bidirectionalBfs(startNodes, depth, limit, permittedConnections).toList
    val dffBuilder = new DataFlowFormatBuilder("Traverse")

    val dffNodes = neoNodes.map { nn =>
      createMainDffNode(nn, dffBuilder)(idProp -> nn.getId)
    }

    assert(dffNodes.size == neoNodes.size)

    val neoToDff = neoNodes.zip(dffNodes).toMap

    assert(neoToDff.size == neoNodes.size)

    for {
      (neoNode, dffNode) <- neoToDff
      rel <- neoNode.getRelationships(Direction.BOTH, permittedConnections.map(DynamicRelationshipType.withName).toArray: _*)
      otherNeo = rel.getOtherNode(neoNode)
      if neoToDff.contains(otherNeo)
      otherDFF = neoToDff(otherNeo)
    } {
      if (rel.getStartNode == neoNode) {
        dffNode --> otherDFF
      } else {
        otherDFF --> dffNode
      }
    }

    val input = dffBuilder.node("Traverse Start").asInput()
    input("limit" -> limit)("depth" -> depth)("start_node" -> startNode.getOrElse(nullParentId))
    input("permitted_connections" -> permittedConnections.mkString("[", ", ", "]"))

    startNodes.map { nn =>
      neoToDff(nn)
    }.foreach { dn =>
      input --> dn
    }

    val output = dffBuilder.node("Traverse Result").asOutput()
    output("size" -> neoNodes.size)
    output("startNodeIds" -> startNodes.map { n => n.getId }.mkString("[", ", ", "]"))

    dffBuilder.toDataFlowFormat
  }
}
