package org.geneticmachine.common.graph

import Graph._

import scala.collection.mutable
import scala.collection.mutable.{ Set => MutableSet }

object GraphBuilder {

  def apply(graphLabel: String) = new GraphBuilder(graphLabel)

  final case class PortBuilder(nodeId: Long, portN: Int)

  final case class NodeBuilder(inputs: Int, outputs: Int, props: mutable.Map[String, Any]) {
    def toNode(edges: Array[Set[Port]]): Node = {
      Node(inputs, outputs, props.toMap, edges)
    }
  }

  case class PortRef(port: PortBuilder, parent: GraphBuilder) {
    def -->(other: PortRef) {
      parent.addEdge(port.nodeId, port.portN, other.port)
    }

    def -->(other: NodeRef) {
      this --> other(0)
    }

    def -/-(other: PortRef) {
      parent.deleteEdge(port.nodeId, port.portN, other.port)
    }
  }

  final case class RelationRef(relationType: String, parent: GraphBuilder) {
    def -*>(graphId: Long): GraphBuilder = {
      parent.relations(relationType) = Set(graphId)
      parent
    }
    
    def -->(graphId: Long): GraphBuilder = {
      val relations = parent.relations.getOrElse(relationType, Set.empty[Long])
      parent.relations(relationType) = relations + graphId
      parent
    }

    def -/-(graph: Long): GraphBuilder = {
      val relations = parent.relations.getOrElse(relationType, Set.empty[Long])
      parent.relations(relationType) = relations - graph
      parent
    }
  }

  object NodeRef {
    def apply(nodeId: Long, parent: GraphBuilder) = new NodeRef(nodeId, parent)
  }

  final class NodeRef(val nodeId: Long, parent: GraphBuilder) extends PortRef(PortBuilder(nodeId, 0), parent) {

    def apply(port: Int): PortRef = {
      PortRef(PortBuilder(nodeId, port), parent)
    }

    def apply(propValue: (String, Any)): NodeRef = {
      parent.nodes(nodeId).props += propValue
      this
    }

    def apply(props: Map[String, Any]): NodeRef = {
      parent.nodes(nodeId).props ++= props
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

import GraphBuilder._

final class GraphBuilder(label: String) {
  private var lastIndex: Long = 0

  private val nodes: mutable.Map[Long, NodeBuilder] = mutable.HashMap.empty[Long, NodeBuilder]
  private val edges: mutable.Map[Long, Array[mutable.Set[PortBuilder]]] = mutable.HashMap.empty[Long, Array[mutable.Set[PortBuilder]]]
  private val relations: mutable.Map[String, Set[Long]] = mutable.Map.empty[String, Set[Long]]

  private val props: mutable.Map[String, Any] = mutable.Map[String, Any](labelProp -> label)

  private var inputNode: Option[NodeRef] = None
  private var outputNode: Option[NodeRef] = None

  def withInput(node: NodeRef): GraphBuilder = {
    inputNode = Some(node)
    node(labelProp -> inputLabel)
    this
  }

  def withOutput(node: NodeRef): GraphBuilder = {
    outputNode = Some(node)
    node(labelProp -> outputLabel)
    this
  }

  def withParent(id: Long): GraphBuilder = {
    this(parentRelation) -*> id
    this
  }

  def withId(id: Long): GraphBuilder = {
    this(idProp -> id)
    this
  }

  def withType(graphType: String): GraphBuilder = {
    this(typeProp -> graphType)
  }

  def apply(relation: String): RelationRef = {
    new RelationRef(relation, this)
  }

  def apply(propValue: (String, Any)): GraphBuilder = {
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

  def toGraph(inputNode: NodeRef, outputNode: NodeRef): Graph = {
    this.withInput(inputNode).withOutput(outputNode).toGraph
  }

  def toGraph: Graph = {
    val linearIndexes = nodes.keys.toList
    val indexTransform = linearIndexes.zipWithIndex.toMap

    def portSetTransform(ports: mutable.Set[PortBuilder]): Set[Port] = {
      (for {
        port <- ports
        PortBuilder(nodeID, portN) = port
      } yield Port(indexTransform(nodeID), portN)).toSet
    }

    val nodeSeq = for {
      index: Long <- linearIndexes
      edgesByPorts: Array[MutableSet[PortBuilder]] = edges(index)
      linearEdges: Array[Set[Port]] = for { ports <- edgesByPorts } yield portSetTransform(ports)
      nodeBuilder = nodes(index)
    } yield nodeBuilder.toNode(linearEdges)

    val inputNodeId = indexTransform(inputNode.get.nodeId)
    val outputNodeId = indexTransform(outputNode.get.nodeId)

    Graph(props.toMap, relations.toMap, nodeSeq.toVector, inputNodeId, outputNodeId)
  }
}