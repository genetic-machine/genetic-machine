package geneticmachine.dataflow

object DataFlowFormat {

  val inputLabel = "INPUT"
  val outputLabel = "OUTPUT"
  val nodeLabel = "NODE"

  val parentRelation = "INHERIT"

  val typeProp = "$type"
  val labelProp = "$label"
  val inputsProp = "$inputs"
  val outputsProp = "$outputs"

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

  def empty(flowLabel: String, inputType: String, outputType: String): DataFlowFormat = {
    val flowBuilder = DataFlowFormatBuilder(flowLabel)
    val inputNode = flowBuilder.node(inputType).asInput()
    val outputNode = flowBuilder.node(outputType).asOutput()
    inputNode(0) --> outputNode(0)
    flowBuilder.toDataFlowFormat
  }

  private def sampleBuilder: DataFlowFormatBuilder = {
    val dffBuilder = DataFlowFormatBuilder("BRAIN").withType("TEST_BRAIN")
    val inputNode = dffBuilder.node("TEST_INPUT")("name" -> "input").asInput()
    val outputNode = dffBuilder.node("TEST_OUTPUT")("name" -> "output").asOutput()
    val n1 = dffBuilder.node("F12", 1, 2)("name" -> "n1")
    val n2 = dffBuilder.node("F21", 2, 1)("name" -> "n2")
    val n3 = dffBuilder.node("G21", 2, 1)("name" -> "n3")

    inputNode(0) --> n1(0)
    inputNode(0) --> n2(1)

    n1(0) --> n2(0)
    n1(1) --> n3(1)
    n2(0) --> n3(0)

    n3(0) --> outputNode(0)

    dffBuilder
  }

  def sample(parentID: Long): DataFlowFormat = {
    sampleBuilder.withParent(parentID).toDataFlowFormat
  }

  def sample: DataFlowFormat = {
    sampleBuilder.toDataFlowFormat
  }
}

import DataFlowFormat._

final case class DataFlowFormat(props: Map[String, Any], relations: Set[(String, Long)],
                                nodes: IndexedSeq[Node], inputNodeId: Int, outputNodeId: Int) {

  def apply(prop: String) = props(prop)

  def node(nodeId: Int): Node = {
    nodes(nodeId)
  }

  override def toString: String = {
    def nodeRepr(node: Node, nodeId: Int): String = {
      nodeId match {
        case `inputNodeId` =>  s"  ->($nodeId) $node"
        case `outputNodeId` => s"  <-($nodeId) $node"
        case _ =>              s"    ($nodeId) $node"
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
    } yield s"  -[$r]-> DataFlow($id)").mkString("\n")

    s"DataFlow {$propsRepr}:\n{$relationsRepr}:\n$nodesRepr"
  }
}