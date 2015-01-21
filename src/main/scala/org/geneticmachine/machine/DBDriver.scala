package org.geneticmachine.machine

import org.geneticmachine.common.graph._

trait DBDriver {
  def save(graph: Graph): Long
  def load(id: Long): Graph

  /**
   * Breadth first traverse over all top-level nodes, i.e. main dff nodes.
   * @param startNodeId Some(start node id) or None for start with nodes
   *                    without [[Graph.parentRelation]].
   * @param depth maximal depth.
   * @param limit maximal number of nodes.
   * @param permittedConnections type of relations to follow.
   * @return graph of top-level nodes (i.e. main dff nodes) relations.
   */
  def traverse(startNodeId: Option[Long], depth: Int,
               limit: Long, permittedConnections: Seq[String]): Graph = {
    Graph.empty("Traverse", "traverseInput", "traverseOutput")
  }

  def shutdown(): Unit
}
