package org.pfcoperez.dailyalgorithm.datastructures.graphs.undirected

case class Edge[Node: Ordering, W](a: Node, b: Node, weight: W) extends Link[Node, W] {
  override val nodeOrdering = implicitly[Ordering[Node]]
}

object Edge {
  def apply[Node: Ordering](a: Node, b: Node): Edge[Node, NoWeight] = Edge(a, b, ())
}

object UndirectedWeighedGraph {

  /**
   * Build a new undirected graphs given its nodes and edges.
   * O(m), m = number of edges provided
   *
   */
  def apply[Node: Ordering, W](nodes: Set[Node], edges: Seq[Edge[Node, W]]): UndirectedWeighedGraph[Node, W] = {

    val (nodeEdges, edgeCounters) = {

      type EdgeType = Edge[Node, W]

      ((Map.empty[Node, Map[Node, EdgeType]], Map.empty[EdgeType, Int]) /: edges) {
        case ((prevRelations, prevCounters), edge @ Edge(a, b, w)) =>
          val updatedRelations = (prevRelations /: Seq(a, b).zip(Seq(b, a))) {
            case (relations, (from, to)) =>
              require((nodes contains from) && (nodes contains to), invalidEdgeError)
              relations + (from -> (relations.getOrElse(from, Map.empty) + (to -> edge)))
          }
          val updatedCounters = prevCounters + (edge -> (prevCounters.getOrElse(edge, 0) + 1))
          (updatedRelations, updatedCounters)
      }

    }

    new UndirectedWeighedGraph(nodes, nodeEdges, edgeCounters)

  }

}

object UndirectedGraph {

  def apply[Node: Ordering](
    nodes: Set[Node],
    edges: Seq[Edge[Node, NoWeight]]): UndirectedWeighedGraph[Node, NoWeight] = UndirectedWeighedGraph(nodes, edges)

}

class UndirectedWeighedGraph[Node, W] private (
  val nodes: Set[Node],
  private val relations: Map[Node, Map[Node, Edge[Node, W]]],
  private val edgeCounters: Map[Edge[Node, W], Int]) extends UndirectedGraphOps[Node, W, Edge[Node, W], UndirectedWeighedGraph[Node, W]] {

  /**
   * Provide the list of edges in the graph.
   * O(m), m = number of edges in the graph
   */
  def edges: Seq[Edge[Node, W]] = edgeCounters.view.toSeq flatMap {
    case (edge, count) =>
      Seq.fill(count)(edge)
  }

  /**
   * Provide the list of edges related to the given node
   * O(m), m = number of edges in the graph.
   */
  def edges(node: Node): Seq[Edge[Node, W]] = relations.get(node) map { edgeMap =>
    edgeMap.toSeq flatMap { case (_, edge) => Seq.fill(edgeCounters(edge))(edge) }
  } getOrElse Seq.empty

  /**
   * Provide the list of adjacent nodes to the given one.
   * O(1)
   */
  def adjacentTo(node: Node): Set[Node] =
    relations.getOrElse(node, Map.empty[Node, Edge[Node, W]]).keySet

  /**
   * Add an edge between two nodes in the graph.
   * O(1)
   */
  def +(edge: Edge[Node, W]): UndirectedWeighedGraph[Node, W] = {
    val Edge(a, b, _) = edge
    val newRelations = (relations /: Seq(a, b).zip(Seq(b, a))) {
      case (updatedRelations, (from, to)) =>
        updatedRelations + (from -> (relations.getOrElse(from, Map.empty[Node, Edge[Node, W]]) + (to -> edge)))
    }
    val updatedCounters = edgeCounters + (edge -> (edgeCounters.getOrElse(edge, 0) + 1))
    new UndirectedWeighedGraph(nodes, newRelations, updatedCounters)
  }

  /**
   * Add a node the graph.
   * O(1)
   */
  def +(node: Node): UndirectedWeighedGraph[Node, W] = {
    new UndirectedWeighedGraph(nodes + node, relations, edgeCounters)
  }

  /*
     * Remove an edge from the graph
     * O(1)
     */
  def -(edge: Edge[Node, W]): UndirectedWeighedGraph[Node, W] = {
    val Edge(a, b, w) = edge
    require(Seq(a, b).forall(nodes contains _), invalidEdgeError)

    val (newRelations, newCounters) = {
      //Decrease the number of this edge counters.
      val newC = edgeCounters.getOrElse(edge, 1) - 1
      if (newC == 0) { // If there are no more:
        val newRelations = (relations /: Seq(a, b).zip(Seq(b, a))) {
          // Then there is no longer a relation between `a` and `b`
          case (updatedRelations, (from, to)) =>
            updatedRelations.get(from) map { adjacentEntries =>
              updatedRelations.updated(from, adjacentEntries - to)
            } getOrElse updatedRelations
        }
        (newRelations, edgeCounters - edge)
      } else (relations, edgeCounters.updated(edge, newC))
    }

    new UndirectedWeighedGraph(nodes, newRelations, newCounters)
  }

  /**
   * Remove a node from the graph
   * O(n+m), n = number of nodes, m = number of edges in the graph
   */
  def -(node: Node): UndirectedWeighedGraph[Node, W] = {
    val newCounters = edgeCounters filterKeys {
      case Edge(a, b, _) => !Set(a, b).contains(node)
    }
    val newRelations = (relations - node) mapValues (_ - node) filter (_._2.nonEmpty)
    new UndirectedWeighedGraph(nodes - node, newRelations, newCounters)
  }

}
