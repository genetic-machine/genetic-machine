package org.geneticmachine.machine.db

import java.util.concurrent.TimeUnit

import org.neo4j.graphdb.factory.GraphDatabaseFactory
import org.neo4j.graphdb.{Node => NeoNode, _}

import scala.collection.JavaConversions._
import scala.util.{Failure, Success, Try}


import org.geneticmachine.common
import org.geneticmachine.machine

import common.graph.Graph._
import common.graph.GraphBuilder.NodeRef
import common.graph._
import machine.DBDriver

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

import Neo4JDriver._

final class Neo4JDriver(val dbPath: String) extends DBDriver {

  val graphDB = new GraphDatabaseFactory().newEmbeddedDatabase(dbPath)

  /** Initialize or create brain index **/
  withTx {
    val algoDlabel = DynamicLabel.label(algorithmLabel)
    val algoIndex = graphDB.schema().getIndexes(algoDlabel)

    if (!algoIndex.exists { ind => ind.getPropertyKeys.toList.contains(parentProp) }) {
      graphDB.schema().indexFor(algoDlabel).on(parentProp).create()
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

  def createNode(node: Node): NeoNode = {
    val props = node.props.updated(inputsProp, node.inputs).updated(outputsProp, node.outputs)
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

  override def save(graph: Graph): Long = withTx {
    val mainNode = createNode {
      val parentId = if (graph.relations.contains(parentRelation)) {
        graph.relations(parentRelation).head
      } else {
        nullParentId
      }

      graph.props + (parentProp -> parentId)
    }

    val graphIdToNeoNode = (for {
      (node, id) <- graph.nodes.zipWithIndex
      neoNode = createNode(node)
    } yield (id, neoNode)).toMap

    for {
      (nodeId, neoNode) <- graphIdToNeoNode
      (outs, port) <- graph.node(nodeId).edges.zipWithIndex
      Port(outNode, outPort) <- outs
    } {
      val outNeoNode = graphIdToNeoNode(outNode)
      val r = neoNode.createRelationshipTo(outNeoNode, connected)
      r.setProperty(portFromProp, port)
      r.setProperty(portToProp, outPort)
    }

    val inputNode = graphIdToNeoNode(graph.inputNodeId)
    val outputNode = graphIdToNeoNode(graph.outputNodeId)

    mainNode.createRelationshipTo(inputNode, write)
    mainNode.createRelationshipTo(outputNode, read)

    for {
      (relationType, ids) <- graph.relations
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

  def createMainGraphNode(neoNode: NeoNode, graphBuilder: GraphBuilder): NodeRef = {
    val ref = graphBuilder.node()

    for {
      prop <- neoNode.getPropertyKeys
      value = neoNode.getProperty(prop)
    } {
      ref(prop -> value)
    }

    ref
  }

  def createNode(neoNode: NeoNode, builder: GraphBuilder): NodeRef = {
    val (inputs, outputs) = getInputsOutputs(neoNode)
    val ref = builder.node(inputs, outputs)

    val propKeys = neoNode.getPropertyKeys.toSet -- Set(inputsProp, outputsProp)

    for {
      key <- propKeys
      value = neoNode.getProperty(key)
    } {
      ref(key -> value)
    }

    ref
  }

  override def load(id: Long): Graph = withTx {
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

    val builder = GraphBuilder(mainNode.getLabels.toList(0).name())

    for {
      prop <- mainNode.getPropertyKeys.toSet -- Set(parentProp)
      value = mainNode.getProperty(prop)
    } {
      builder(prop -> value)
    }

    val neoToGraph = (for {
      neoNode <- neoNodes
      node = createNode(neoNode, builder)
    } yield (neoNode, node)).toMap

    for {
      (neoNode, startNode) <- neoToGraph
      rel <- neoNode.getRelationships(Direction.OUTGOING, connected).toList
      endNode = neoToGraph(rel.getEndNode)
      (portFrom, portTo) = getPorts(rel)
    } {
      startNode(portFrom) --> endNode(portTo)
    }

    /** ID injection **/
    builder.withId(id)

    builder.withInput(neoToGraph(inputNode)).withOutput(neoToGraph(outputNode)).toGraph
  }

  def getStartNodes(startNodeId: Option[Long], limit: Int): Set[NeoNode] = {
    if (startNodeId.isEmpty) {
      graphDB.findNodesByLabelAndProperty(DynamicLabel.label(algorithmLabel), parentProp,
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
                        limit: Long, permittedConnections: Seq[String]): Graph = withTx {
    val startNodes = getStartNodes(startNode, limit.toInt)

    val neoNodes = bidirectionalBfs(startNodes, depth, limit, permittedConnections).toList
    val builder = new GraphBuilder("Traverse")

    val nodes = neoNodes.map { nn =>
      createMainGraphNode(nn, builder)(idProp -> nn.getId)
    }

    assert(nodes.size == neoNodes.size)

    val neoToGraph = neoNodes.zip(nodes).toMap

    assert(neoToGraph.size == neoNodes.size)

    for {
      (neoNode, node) <- neoToGraph
      rel <- neoNode.getRelationships(Direction.BOTH, permittedConnections.map(DynamicRelationshipType.withName).toArray: _*)
      otherNeo = rel.getOtherNode(neoNode)
      if neoToGraph.contains(otherNeo)
      otherGraph = neoToGraph(otherNeo)
    } {
      if (rel.getStartNode == neoNode) {
        node --> otherGraph
      } else {
        otherGraph --> node
      }
    }

    val input = builder.node("Traverse Start").asInput()
    input("limit" -> limit)("depth" -> depth)("start_node" -> startNode.getOrElse(nullParentId))
    input("permitted_connections" -> permittedConnections.mkString("[", ", ", "]"))

    startNodes.map { nn =>
      neoToGraph(nn)
    }.foreach { dn =>
      input --> dn
    }

    val output = builder.node("Traverse Result").asOutput()
    output("size" -> neoNodes.size)
    output("startNodeIds" -> startNodes.map { n => n.getId }.mkString("[", ", ", "]"))

    builder.toGraph
  }
}
