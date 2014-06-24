package geneticmachine.ubf

import scala.concurrent.Future

import scala.collection.mutable

object UnifiedBrainFormat {
  final case class Port(nodeId: Int, portN: Int)

  final case class Node(inputs: Int, outputs: Int, props: Map[String, Any], edges: Array[Set[Port]]) {
    override def toString: String = {
      val edgesRepr = (for {
        (ports, portN) <- edges.zipWithIndex
        portsRepr = (for { port <- ports } yield s"${port.nodeId}[${port.portN}]").mkString(", ")
      } yield s"    [$portN]: {$portsRepr}").mkString("\n")

      val propsRepr = (for {
        (k, v) <- props
      } yield s"$k = $v").mkString(", ")

      s": {$propsRepr}\n$edgesRepr"
    }
  }

  def empty(brainType: String, inputType: String, outputType: String, parentID: Long = -1): UnifiedBrainFormat = {
    val ubfBuilder = UnifiedBrainFormatBuilder(brainType, parentID)
    val inputNode = ubfBuilder.node(inputType)
    val outputNode = ubfBuilder.node(outputType)
    inputNode(0) --> outputNode(0)
    ubfBuilder.toUBF(inputNode, outputNode)
  }

  def sample(parentID: Long = -1): UnifiedBrainFormat = {
    val ubfBuilder = UnifiedBrainFormatBuilder("SAMPLE", parentID)
    val inputNode = ubfBuilder.node("TEST_INPUT")("name" -> "input")
    val outputNode = ubfBuilder.node("TEST_OUTPUT")("name" -> "output")
    val n1 = ubfBuilder.node("NODE", 1, 2)("name" -> "n1")
    val n2 = ubfBuilder.node("NODE", 2, 1)("name" -> "n2")
    val n3 = ubfBuilder.node("NODE", 2, 1)("name" -> "n3")

    inputNode(0) --> n1(0)
    inputNode(0) --> n2(1)

    n1(0) --> n2(0)
    n1(1) --> n3(1)
    n2(0) --> n3(0)

    n3(0) --> outputNode(0)

    ubfBuilder.toUBF(inputNode, outputNode)
  }
}

import UnifiedBrainFormat._

final case class UnifiedBrainFormat(brainType: String, parentID: Long, nodes: IndexedSeq[Node], inputNodeId: Int, outputNodeId: Int) {

  def node(nodeId: Int): Node = {
    nodes(nodeId)
  }

  override def toString: String = {
    val nodesRepr = (for {
      (node, nodeId) <- nodes.zipWithIndex
    } yield s"  ($nodeId) $node").mkString("\n")

    s"Brain: $brainType, parent: $parentID, ($inputNodeId) -...-> ($outputNodeId):\n$nodesRepr"
  }
}

object UnifiedBrainFormatBuilder {

  final case class PortBuilder(nodeId: Long, portN: Int)

  final case class NodeBuilder(inputs: Int, outputs: Int, props: mutable.Map[String, Any]) {
    def toNode(edges: Array[Set[Port]]): Node = {
      Node(inputs, outputs, props.toMap, edges)
    }
  }

  final case class PortRef(port: PortBuilder, parent: UnifiedBrainFormatBuilder) {
    def -->(other: PortRef) {
      if (other.port.portN < 0 || other.port.portN >= parent.nodes(other.port.nodeId).inputs) {
        throw new NoSuchElementException("No such port")
      }

      parent.edges(port.nodeId)(port.portN).add(other.port)
    }

    def -/-(other: PortRef) {
      parent.edges(port.nodeId)(port.portN).remove(other.port)
    }
  }

  final case class NodeRef(nodeId: Long, parent: UnifiedBrainFormatBuilder) {

    if (!parent.nodes.contains(nodeId)) {
      throw new NoSuchElementException(s"No such node $nodeId")
    }

    def apply(port: Int): PortRef = {
      PortRef(PortBuilder(nodeId, port), parent)
    }

    def apply(propValue: (String, Any)): NodeRef = {
      parent.nodes(nodeId).props += ((propValue._1, propValue._2))
      this
    }

    def \(prop: String): NodeRef = {
      parent.nodes(nodeId).props.remove(prop)
      this
    }

    def delete() {
      parent.nodes.remove(nodeId)
    }
  }
}

import UnifiedBrainFormatBuilder._

final case class UnifiedBrainFormatBuilder(brainType: String, parentID: Long) {
  var lastIndex: Long = 0

  val nodes: mutable.HashMap[Long, NodeBuilder] = mutable.HashMap.empty[Long, NodeBuilder]
  val edges: mutable.HashMap[Long, Array[mutable.Set[PortBuilder]]] = mutable.HashMap.empty[Long, Array[mutable.Set[PortBuilder]]]

  def node(nodeType: String, inputs: Int = 1, outputs: Int = 1): NodeRef = {
    val nodeId: Long = lastIndex

    nodes(nodeId) = NodeBuilder(inputs, outputs, mutable.Map("$type" -> nodeType))
    edges(nodeId) = Array.fill(outputs) { mutable.Set.empty[PortBuilder] }

    val ref = NodeRef(nodeId, this)

    lastIndex += 1

    ref
  }

  def getNode(id: Long): NodeRef = {
    NodeRef(id, this)
  }

  def toUBF(inputNode: NodeRef, outputNode: NodeRef): UnifiedBrainFormat = {
    val linearIndexes = nodes.keys.toVector
    val indexTransform = linearIndexes.zipWithIndex.toMap

    def portSetTransform(ports: mutable.Set[PortBuilder]): Set[Port] = {
      (for {
        port <- ports
        PortBuilder(nodeID, portN) = port
      } yield Port(indexTransform(nodeID), portN)).toSet
    }

    val nodeSeq = for {
      index: Long <- linearIndexes
      edgesByPorts: Array[mutable.Set[PortBuilder]] = edges(index)
      linearEdges: Array[Set[Port]] = for { ports <- edgesByPorts } yield portSetTransform(ports)
      nodeBuilder = nodes(index)
    } yield nodeBuilder.toNode(linearEdges)

    val inputNodeId = indexTransform(inputNode.nodeId)
    val outputNodeId = indexTransform(outputNode.nodeId)

    UnifiedBrainFormat(brainType, parentID, nodeSeq, inputNodeId, outputNodeId)
  }
}

trait UnifiedBrainFormatDriver {
  def save(ubf: UnifiedBrainFormat): Future[Long]
  def load(id: Long): Future[UnifiedBrainFormat]
}