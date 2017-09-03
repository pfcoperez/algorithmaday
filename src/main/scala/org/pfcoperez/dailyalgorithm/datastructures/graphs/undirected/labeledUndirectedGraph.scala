package org.pfcoperez.dailyalgorithm.datastructures.graphs.undirected

object labeled {

  case class Edge[L, Node: Ordering, W](label: L, a: Node, b: Node, weight: W) extends Link[Node, W] {
    override val nodeOrdering = implicitly[Ordering[Node]]
    override def key: Any = label
  }

  object Edge {
    def apply[L, Node: Ordering](label: L, a: Node, b: Node): Edge[L, Node, NoWeight] =
      Edge(label, a, b, ())
  }

  object UndirectedWeighedGraph {

    /**
     * Build a new undirected graphs given its nodes and edges.
     * O(m), m = number of edges provided
     *
     */
    def apply[L, Node: Ordering, W](nodes: Set[Node], edges: Seq[Edge[L, Node, W]]): UndirectedWeighedGraph[L, Node, W] = {
      type EdgeType = Edge[L, Node, W]
      (new UndirectedWeighedGraph(nodes, Map.empty[Node, Map[Node, EdgeType]], Set.empty[EdgeType]) /: edges)(_ + _)
    }

  }

  object UndirectedGraph {

    def apply[L, Node: Ordering](
      nodes: Set[Node],
      edges: Seq[Edge[L, Node, NoWeight]]): UndirectedWeighedGraph[L, Node, NoWeight] = UndirectedWeighedGraph(nodes, edges)
  }

  class UndirectedWeighedGraph[L, Node, W] private (
    val nodes: Set[Node],
    private val relations: Map[Node, Map[Node, Edge[L, Node, W]]],
    private val edgeSet: Set[Edge[L, Node, W]]) extends UndirectedGraphOps[Node, W, Edge[L, Node, W], UndirectedWeighedGraph[L, Node, W]] {

    /**
     * Provide the list of edges in the graph.
     * O(1)
     */
    def edges(): Seq[Edge[L, Node, W]] = edgeSet.view.toSeq

    /**
     * Provide the list of edges related to the given node
     * O(1)
     */
    def edges(node: Node): Seq[Edge[L, Node, W]] =
      relations.getOrElse(node, Map.empty).values.toSeq

    /**
     * Provide the list of adjacent nodes to the given one.
     * O(1)
     */
    def adjacentTo(node: Node): Set[Node] =
      relations.getOrElse(node, Map.empty).keySet

    /**
     * Add an edge between two nodes in the graph.
     * O(1)
     */
    def +(edge: Edge[L, Node, W]): UndirectedWeighedGraph[L, Node, W] = {
      val Edge(_, a, b, _) = edge
      val newRelations = (relations /: Seq(a, b).zip(Seq(b, a))) {
        case (updatedRelations, (from, to)) =>
          updatedRelations + (from -> (relations.getOrElse(from, Map.empty[Node, Edge[L, Node, W]]) + (to -> edge)))
      }
      new UndirectedWeighedGraph(nodes, newRelations, edgeSet + edge)
    }

    /**
     * Add a node the graph.
     * O(1)
     */
    def +(node: Node): UndirectedWeighedGraph[L, Node, W] =
      new UndirectedWeighedGraph(nodes + node, relations, edgeSet)

    /**
     * Remove an edge from the graph
     * O(1)
     */
    def -(edge: Edge[L, Node, W]): UndirectedWeighedGraph[L, Node, W] = {
      val Edge(_, a, b, _) = edge
      require(Seq(a, b).forall(nodes contains _), invalidEdgeError)

      val newRelations = (relations /: Seq(a, b).zip(Seq(b, a))) {
        case (updatedRelations, (from, to)) =>
          updatedRelations.get(from) map { adjacentEntries =>
            updatedRelations.updated(from, adjacentEntries - to)
          } getOrElse updatedRelations
      }

      new UndirectedWeighedGraph(nodes, newRelations, edgeSet - edge)
    }

    /**
     * Remove a node from the graph
     * O(m), n = number of nodes, m = number of edges in the graph
     */
    def -(node: Node): UndirectedWeighedGraph[L, Node, W] = {
      val newEdgeSet = edgeSet filter {
        case Edge(_, a, b, _) => !Set(a, b).contains(node)
      }
      val newRelations = (relations - node) mapValues (_ - node) filter (_._2.nonEmpty)
      new UndirectedWeighedGraph(nodes - node, newRelations, newEdgeSet)
    }

  }

}
