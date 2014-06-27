package geneticmachine.dataflow

import DataFlowFormat._

import scala.collection.mutable

object DataFlowFormatBuilder {

  def apply(flowLabel: String) = new DataFlowFormatBuilder(flowLabel)

  final case class PortBuilder(nodeId: Long, portN: Int)

  final case class NodeBuilder(inputs: Int, outputs: Int, props: mutable.Map[String, Any]) {
    def toNode(edges: Array[Set[Port]]): Node = {
      Node(inputs, outputs, props.toMap, edges)
    }
  }

  final case class PortRef(port: PortBuilder, parent: DataFlowFormatBuilder) {
    def -->(other: PortRef) {
      parent.addEdge(port.nodeId, port.portN, other.port)
    }

    def -/-(other: PortRef) {
      parent.deleteEdge(port.nodeId, port.portN, other.port)
    }
  }

  final case class FlowRelationRef(relationType: String, parent: DataFlowFormatBuilder) {
    def -->(flowId: Long): DataFlowFormatBuilder = {
      parent.relations.add((relationType, flowId))
      parent
    }

    def -/-(flowId: Long): DataFlowFormatBuilder = {
      parent.relations.remove((relationType, flowId))
      parent
    }
  }

  final case class NodeRef(nodeId: Long, parent: DataFlowFormatBuilder) {
    def apply(port: Int): PortRef = {
      PortRef(PortBuilder(nodeId, port), parent)
    }

    def apply(propValue: (String, Any)): NodeRef = {
      parent.nodes(nodeId).props += propValue
      this
    }

    def withType(nodeType: String): NodeRef = {
      apply(typeProp -> nodeType)
    }

    def \(prop: String): NodeRef = {
      parent.nodes(nodeId).props.remove(prop)
      this
    }

    def delete() {
      parent.deleteNode(nodeId)
    }

    def asInput(): NodeRef = {
      parent.withInput(this)
      this
    }

    def asOutput(): NodeRef = {
      parent.withOutput(this)
      this
    }
  }
}

import DataFlowFormatBuilder._

final class DataFlowFormatBuilder(label: String) {
  private var lastIndex: Long = 0

  private val nodes: mutable.Map[Long, NodeBuilder] = mutable.LongMap.empty[NodeBuilder]
  private val edges: mutable.Map[Long, Array[mutable.Set[PortBuilder]]] = mutable.LongMap.empty[Array[mutable.Set[PortBuilder]]]
  private val relations: mutable.Set[(String, Long)] = mutable.Set.empty[(String, Long)]

  private val props: mutable.Map[String, Any] = mutable.Map[String, Any](labelProp -> label)

  private var inputNode: Option[NodeRef] = None
  private var outputNode: Option[NodeRef] = None

  def withInput(node: NodeRef): DataFlowFormatBuilder = {
    inputNode = Some(node)
    node(labelProp -> inputLabel)
    this
  }

  def withOutput(node: NodeRef): DataFlowFormatBuilder = {
    outputNode = Some(node)
    node(labelProp -> outputLabel)
    this
  }

  def withParent(id: Long): DataFlowFormatBuilder = {
    this(parentRelation) --> id
    this
  }

  def withType(dffType: String): DataFlowFormatBuilder = {
    this(typeProp -> dffType)
  }

  def apply(relation: String): FlowRelationRef = {
    new FlowRelationRef(relation, this)
  }

  def apply(propValue: (String, Any)): DataFlowFormatBuilder = {
    props(propValue._1) = propValue._2
    this
  }

  def \(prop: String) {
    props.remove(prop)
  }

  def node(nodeType: String): NodeRef = {
    node(1, 1)(typeProp -> nodeType)
  }

  def node(nodeType: String, inputs: Int, outputs: Int): NodeRef = {
    node(inputs, outputs)(typeProp -> nodeType)
  }

  def node(inputs: Int, outputs: Int): NodeRef = {
    val nodeId: Long = lastIndex

    nodes(nodeId) = NodeBuilder(inputs, outputs, mutable.Map.empty)
    edges(nodeId) = Array.fill(outputs) { mutable.Set.empty[PortBuilder] }

    val ref = NodeRef(nodeId, this)

    lastIndex += 1

    ref
  }

  def node(): NodeRef = {
    node(1, 1)
  }

  def node(nodeId: Long): NodeRef = {
    if (!nodes.contains(nodeId)) {
      throw new NoSuchElementException("No such node")
    }

    NodeRef(nodeId, this)
  }

  def deleteNode(nodeId: Long) {
    nodes.remove(nodeId)
    edges.remove(nodeId)
  }

  def addEdge(nodeFromId: Long, portFrom: Int, portTo: PortBuilder) {
    if (nodes(portTo.nodeId).inputs <= portTo.portN) {
      throw new NoSuchElementException("No such port!")
    }

    edges(nodeFromId)(portFrom).add(portTo)
  }

  def deleteEdge(nodeFromId: Long, portFrom: Int, portTo: PortBuilder) {
    edges(nodeFromId)(portFrom).remove(portTo)
  }

  def toDataFlowFormat(inputNode: NodeRef, outputNode: NodeRef): DataFlowFormat = {
    this.withInput(inputNode).withOutput(outputNode).toDataFlowFormat
  }

  def toDataFlowFormat: DataFlowFormat = {
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

    val inputNodeId = indexTransform(inputNode.get.nodeId)
    val outputNodeId = indexTransform(outputNode.get.nodeId)

    DataFlowFormat(props.toMap, relations.toSet, nodeSeq, inputNodeId, outputNodeId)
  }
}
