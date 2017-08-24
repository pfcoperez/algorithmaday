package org.pfcoperez.dailyalgorithm.datastructures.graphs

package object undirected {

  type NoWeight = Unit

  type UndirectedGraph[Node] = UndirectedWeighedGraph[Node, NoWeight]

  trait Link[Node, W] {

    val a: Node
    val b: Node
    val weight: W

    protected implicit val nodeOrdering: Ordering[Node]

    def connectedNodes: Seq[Node] = Seq(a, b).sorted

    protected def key: Any = (connectedNodes, weight)
    override def hashCode: Int = key.hashCode
    override def equals(that: Any): Boolean = that match {
      case that: Link[Node, W] => key == that.key
      case _ => false
    }

  }

  trait UndirectedGraphOps[Node, W, EdgeType <: Link[Node, W], UndirectedGraph <: UndirectedGraphOps[Node, W, EdgeType, _]] {

    /**
     * Provide the list of edges in the graph.
     */
    def edges(): Seq[EdgeType]

    /**
     * Provide the list of edges related to the given node
     */
    def edges(node: Node): Seq[EdgeType]
    /**
     * Provide the list of adjacent nodes to the given one.
     */
    def adjacentTo(node: Node): Set[Node]

    /**
     * Add an edge between two nodes in the graph.
     */
    def +(edge: EdgeType): UndirectedGraph

    /**
     * Add a node the graph.
     */
    def +(node: Node): UndirectedGraph

    /*
     * Remove an edge from the graph
     */
    def -(edge: EdgeType): UndirectedGraph

    /**
     * Remove a node from the graph
     */
    def -(node: Node): UndirectedGraph

  }

  private[undirected] val invalidEdgeError = "Edges should connect nodes in the graph"

}
