package org.geneticmachine.common.graph

import scala.reflect._

object Graph {

  val algorithmLabel = "ALGORITHM"
  val environmentLabel = "ENVIRONMENT"

  val inputLabel = "INPUT"
  val outputLabel = "OUTPUT"
  val nodeLabel = "NODE"

  val parentRelation = "INHERIT"
  val experimentRelation = "EXAMINES"

  val typeProp = "$type"
  val labelProp = "$label"

  val inputsProp = "$inputs"
  val outputsProp = "$outputs"

  val idProp = "$id"
  val errorCauseProp = "$cause"
  val nullParent: Long = -1l

  final case class Port(nodeId: Int, portN: Int)

  final case class Node(inputs: Int, outputs: Int, props: Map[String, Any], edges: Array[Set[Port]]) {
    override def toString: String = {
      val edgesRepr = (for {
        (ports, portN) <- edges.zipWithIndex
        portsRepr = (for { port <- ports } yield s"${port.nodeId}[${port.portN}]").mkString(", ")
      } yield s"        [$portN]: {$portsRepr}").mkString("\n")

      val propsRepr = (for {
        (k, v) <- props
      } yield s"$k = $v").mkString(", ")

      s"[$inputs -> $outputs] {$propsRepr}\n$edgesRepr"
    }
  }

  def empty(graphLabel: String, inputType: String, outputType: String): Graph = {
    val builder = GraphBuilder(graphLabel)

    val inputNode = builder.node(inputType).asInput()
    val outputNode = builder.node(outputType).asOutput()
    inputNode --> outputNode

    builder.toGraph
  }

  def single(graphLabel: String)(nodeType: String, props: Map[String, Any]): Graph = {
    val builder = new GraphBuilder(graphLabel)

    builder.node(nodeType)(props).asInput().asOutput()

    builder.toGraph
  }

  def errorGraph(graphLabel: String, e: Throwable): Graph = {
    val props = Map(errorCauseProp -> e.getStackTrace.mkString("\n"))

    single(graphLabel)("Error", props)
  }

  final class Props(val props: Map[String, Any]) {
    def getAs[T : ClassTag](key: String): Option[T] = {
      for {
        x <- props.get(key)
        if classTag[T].runtimeClass.isInstance(x)
      } yield x.asInstanceOf[T]
    }
  }

  import scala.language.implicitConversions
  implicit def mapToProps(props: Map[String, Any]): Props = new Props(props)
}

import Graph._

final case class Graph(props: Map[String, Any], relations: Map[String, Set[Long]],
                       nodes: Vector[Node], inputNodeId: Int, outputNodeId: Int) {

  def apply(prop: String): Any = props(prop)
  def apply(nodeId: Int): Node = nodes(nodeId)
  def node(nodeId: Int): Node = nodes(nodeId)

  def id: Option[Long] = props.getAs[Long](idProp)

  override def toString: String = {
    def nodeRepr(node: Node, nodeId: Int): String = {
      nodeId match {
        case _ if nodeId == outputNodeId && nodeId == inputNodeId =>
          s"  ->($nodeId)->  $node"

        case `inputNodeId` =>
          s"  ->($nodeId)    $node"

        case `outputNodeId` =>
          s"    ($nodeId)->  $node"

        case _ =>
          s"    ($nodeId)    $node"
      }
    }

    val nodesRepr = (for {
      (node, nodeId) <- nodes.zipWithIndex
    } yield nodeRepr(node, nodeId)).mkString("\n")

    val propsRepr = (for {
      (k, v) <- props
    } yield s"$k = $v").mkString(", ")

    val relationsRepr = (for {
      (r, id) <- relations
    } yield s"  -[$r]-> Graph($id)").mkString("\n")

    s"Graph {$propsRepr}:\n{$relationsRepr}:\n$nodesRepr"
  }

  def injection(propName: String, value: Any): Graph = {
    this.copy(props = props + (propName -> value))
  }

  def idInjection(id: Long): Graph = {
    this.copy(props = props + (idProp -> id))
  }

  def relationInjection(relationType: String, id: Long): Graph = {
    val relations = this.relations.getOrElse(relationType, Set.empty)
    val injected = this.relations.updated(relationType, relations + id)
    this.copy(relations = injected)
  }

  def uniqueRelationInjection(relationType: String, id: Long): Graph = {
    val injected = this.relations.updated(relationType, Set(id))
    this.copy(relations = injected)
  }

  def parentInjection(id: Long): Graph = {
    uniqueRelationInjection(parentRelation, id)
  }
}