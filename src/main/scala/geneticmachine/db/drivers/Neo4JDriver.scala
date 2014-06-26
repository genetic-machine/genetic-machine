package geneticmachine.db.drivers

import geneticmachine.ubf.UnifiedBrainFormatBuilder.NodeRef
import geneticmachine.ubf.{UnifiedBrainFormat => UBF, UnifiedBrainFormatBuilder}
import geneticmachine.ubf.UnifiedBrainFormat.{ Node => UBFNode, Port}

import org.neo4j.graphdb._
import org.neo4j.graphdb.factory.GraphDatabaseFactory
import org.neo4j.graphdb.{ Node => NeoNode }

import scala.util.{Success, Try, Failure}

import scala.collection.JavaConversions._

object Neo4JDriver {

  val connected = DynamicRelationshipType.withName("CONNECT")
  val write = DynamicRelationshipType.withName("WRITE")
  val read = DynamicRelationshipType.withName("READ")
  val inherit = DynamicRelationshipType.withName("INHERIT")

  val brainLabel = DynamicLabel.label("BRAIN")
  val nodeLabel = DynamicLabel.label("NODE")
  val inputLabel = DynamicLabel.label("INPUT")
  val outputLabel = DynamicLabel.label("OUTPUT")
}

final class Neo4JDriver(val dbPath: String) extends DBDriver {

  import Neo4JDriver._

  val graphDB = new GraphDatabaseFactory().newEmbeddedDatabase(dbPath)

  val shutdownHook = scala.sys.addShutdownHook {
    graphDB.shutdown()
  }

  def shutdown() = { shutdownHook.remove(); graphDB.shutdown() }

  def createNode(ubfNode: UBFNode, label: Label): NeoNode = {
    val neoNode = graphDB.createNode(label)

    for {
      (k, v) <- ubfNode.props
    } {
      neoNode.setProperty(k, v)
    }

    neoNode.setProperty("$inputs", ubfNode.inputs)
    neoNode.setProperty("$outputs", ubfNode.outputs)

    neoNode
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

  override def saveBrain(ubf: UBF): Long = withTx {

    val brainNode = graphDB.createNode(brainLabel)
    brainNode.setProperty("$type", ubf.brainType)

    def getLabel(id: Int): Label = id match {
      case ubf.inputNodeId => inputLabel
      case ubf.outputNodeId => outputLabel
      case _ => nodeLabel
    }

    val ubfIdToNeoNode = (for {
      (ubfNode, ubfId) <- ubf.nodes.zipWithIndex
      label = getLabel(ubfId)
      neoNode = createNode(ubfNode, label)
    } yield (ubfId, neoNode)).toMap

    for {
      (ubfNodeId, neoNode) <- ubfIdToNeoNode
      (outs, port) <- ubf.node(ubfNodeId).edges.zipWithIndex
      Port(outNode, outPort) <- outs
    } {
      val outNeoNode = ubfIdToNeoNode(outNode)
      val r = neoNode.createRelationshipTo(outNeoNode, connected)
      r.setProperty("$portFrom", port)
      r.setProperty("$portTo", outPort)
    }

    val inputNode = ubfIdToNeoNode(ubf.inputNodeId)
    val outputNode = ubfIdToNeoNode(ubf.outputNodeId)

    brainNode.createRelationshipTo(inputNode, write)
    brainNode.createRelationshipTo(outputNode, read)

    Try {
      val parentNode = graphDB.getNodeById(ubf.parentID)
      brainNode.createRelationshipTo(parentNode, inherit)
    }

    brainNode.getId
  }

  private def getType(node: NeoNode): String = {
    node.getProperty("$type").asInstanceOf[String]
  }

  private def getInputsOutputs(node: NeoNode): (Int, Int) = {
    val ins = node.getProperty("$inputs").asInstanceOf[Int]
    val outs = node.getProperty("$outputs").asInstanceOf[Int]
    (ins, outs)
  }

  private def getPorts(rel: Relationship): (Int, Int) = {
    val portFrom = rel.getProperty("$portFrom").asInstanceOf[Int]
    val portTo = rel.getProperty("$portTo").asInstanceOf[Int]
    (portFrom, portTo)
  }

  override def loadBrain(id: Long): UBF = withTx {
    val brain = graphDB.getNodeById(id)

    def getOnlyConnectedNode(node: NeoNode, rel: RelationshipType): NeoNode = {
      val rels = node.getRelationships(Direction.OUTGOING, rel).toList
      if (rels.length != 1) {
        throw new NotFoundException("No or multiple inputs!")
      }

      rels(0).getEndNode
    }

    val inputNode = getOnlyConnectedNode(brain, write)
    val outputNode = getOnlyConnectedNode(brain, read)

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

    val parentId: Long = {
      val parents = brain.getRelationships(Direction.OUTGOING, inherit).toList
      if (parents.size > 0) {
        parents(0).getEndNode.getId
      } else {
        -1
      }
    }

    val ubfBuilder = UnifiedBrainFormatBuilder(getType(brain), parentId)

    def createUBFNode(neoNode: NeoNode): NodeRef = {
      val (inputs, outputs) = getInputsOutputs(neoNode)
      val ref = ubfBuilder.node(getType(neoNode), inputs, outputs)

      val propKeys = neoNode.getPropertyKeys.toSet.filter { k => !k.startsWith("$") }
      val props = for {
        key <- propKeys
        value = neoNode.getProperty(key)
      } {
        ref(key -> value)
      }

      ref
    }

    val neoToUbf = (for {
      neoNode <- neoNodes
      ubfNode = createUBFNode(neoNode)
    } yield (neoNode, ubfNode)).toMap

    for {
      (neoNode, startUbfNode) <- neoToUbf
      rel <- neoNode.getRelationships(Direction.OUTGOING, connected).toList
      endUbfNode = neoToUbf(rel.getEndNode)
      (portFrom, portTo) = getPorts(rel)
    } {
      startUbfNode(portFrom) --> endUbfNode(portTo)
    }

    ubfBuilder.toUBF(neoToUbf(inputNode), neoToUbf(outputNode))
  }
}
