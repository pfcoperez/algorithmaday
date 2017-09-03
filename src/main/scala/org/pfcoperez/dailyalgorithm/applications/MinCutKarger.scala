package org.pfcoperez.dailyalgorithm.applications

import scala.language.postfixOps

object MinCutKarger {

  import org.pfcoperez.dailyalgorithm.numericmethods.random.impl.IntRandom
  import org.pfcoperez.dailyalgorithm.numericmethods.random.RandomGenerator
  import org.pfcoperez.dailyalgorithm.datastructures.graphs.undirected._

  /*
   * Randomized minimum cut.
   * Given an undirected connected node, finds a sequence of nodes to
   * be removed in order to make a partition.
   * O(n^2 m) [Worst case], n = number of nodes, m = number of edges
   *
   */
  def minCut[Node: Ordering, W](
    ug: UndirectedWeighedGraph[Node, W],
    randomSeed: Long): Option[Seq[Edge[Node, W]]] = {

    type EdgeType = Edge[Node, W]

    val labels = ug.edges.toSet.zipWithIndex map {
      case (node, label) => node -> Set(label)
    } toMap

    /**
     * Merge two nodes keeping track of the original
     * labels.
     *
     * O(nm) [Worst case], n = number of nodes, m = number of edges
     */
    def merge(
      ug: UndirectedWeighedGraph[Node, W],
      edge2label: Map[EdgeType, Set[Int]])(
      assimilated: Node, host: Node): (UndirectedWeighedGraph[Node, W], Map[EdgeType, Set[Int]]) = {
      // The assimilated node gets deleted, its edges assimilated as ...
      (((ug - assimilated), edge2label) /: ug.edges(assimilated)) {
        case ((updatedGraph, labels), edge @ Edge(from, to, w)) =>
          Seq(from, to).find(!Set(assimilated, host).contains(_)) map { outNode =>
            val updatedEdge = Edge(host, outNode, w) // ...new ones

            val updatedLabels = (labels - edge) + { // Track original graph labels
              (updatedEdge, labels.getOrElse(updatedEdge, Set.empty) ++ labels.getOrElse(edge, Set.empty))
            }

            // The new edge is added to the updated graph, without the original edge.
            ((updatedGraph + updatedEdge), updatedLabels)

          } getOrElse ((updatedGraph, labels))
      }
    }

    def minCut(
      state: (UndirectedWeighedGraph[Node, W], Map[EdgeType, Set[Int]]),
      randomGenerator: RandomGenerator[Int]): Option[Seq[Edge[Node, W]]] = {

      val (currentUg, currentLabels) = state

      // How many nodes does the current graph have?
      currentUg.nodes.size match {
        case 0 | 1 => None // It is possible to find a partition of one node graph.
        case 2 => // If only two node remain, a partition is created when disconnecting one from the other.
          val labelsSet = currentUg.edges.flatMap(currentLabels).toSet
          Some {
            // These two nodes are, indeed, classes of nodes of the original graph, the edges
            // connecting them are the edges that should be removed to create the partition.
            ug.edges.filter(edge => labelsSet.contains(labels(edge).head))
          } filter (_.nonEmpty)
        case _ => // There are more than 2 nodes, the graph hasn't been simplified enough so...
          val edges = currentUg.edges.toVector
          val (newGenerator, randomInteger) = randomGenerator.next
          // Choose a random edge
          val edge @ Edge(a, b, _) = edges(Math.abs(randomInteger) % edges.size)
          // And run the algorithm with the graph resulting from merging
          // the nodes connected by the randomly chosen edge.
          minCut(merge(currentUg, currentLabels)(a, b), newGenerator)
      }

    }
    minCut((ug, labels), new IntRandom(randomSeed))
  }

  /*
   * Randomized minimum cut.
   * Given an undirected connected node, finds a sequence of nodes to
   * be removed in order to make a partition.
   * O(n^2 m) [Worst case], n = number of nodes, m = number of edges
   *
   */
  def minCut[L, Node: Ordering, W](
    ug: labeled.UndirectedWeighedGraph[L, Node, W],
    randomSeed: Long): Option[Seq[labeled.Edge[L, Node, W]]] = {

    import labeled._

    type EdgeType = Edge[L, Node, W]

    /**
     * Merge two nodes keeping track of the original
     * labels.
     *
     * O(n+m) [Worst case], n = number of nodes, m = number of edges
     */
    def merge(ug: UndirectedWeighedGraph[L, Node, W])(
      assimilated: Node, host: Node): UndirectedWeighedGraph[L, Node, W] = {
      // The assimilated node gets deleted, its edges assimilated as new ones.
      ((ug - assimilated) /: ug.edges(assimilated)) {
        case (updatedGraph, edge @ Edge(label, from, to, w)) =>
          Seq(from, to).find(!Set(assimilated, host).contains(_)) map { outNode =>
            updatedGraph + Edge(label, host, outNode, w)
          } getOrElse updatedGraph
      }
    }

    def minCut(
      currentUg: UndirectedWeighedGraph[L, Node, W],
      randomGenerator: RandomGenerator[Int]): Option[Seq[Edge[L, Node, W]]] = {

      // How many nodes does the current graph have?
      currentUg.nodes.size match {
        case 0 | 1 => None // It is possible to find a partition of one node graph.
        case 2 => // If only two node remain, a partition is created when disconnecting one from the other.
          val labelsSet = currentUg.edges.map(_.label).toSet
          Some {
            // These two nodes are, indeed, classes of nodes of the original graph, the edges
            // connecting them are the edges that should be removed to create the partition.
            ug.edges filter (labelsSet contains _.label)
          } filter (_.nonEmpty)
        case _ => // There are more than 2 nodes, the graph hasn't been simplified enough so...
          val edges = currentUg.edges.toVector
          val (newGenerator, randomInteger) = randomGenerator.next
          // Choose a random edge
          val edge @ Edge(label, a, b, _) = edges(Math.abs(randomInteger) % edges.size)
          // And run the algorithm with the graph resulting from merging
          // the nodes connected by the randomly chosen edge.
          minCut(merge(currentUg)(a, b), newGenerator)
      }

    }
    minCut(ug, new IntRandom(randomSeed))
  }

}
