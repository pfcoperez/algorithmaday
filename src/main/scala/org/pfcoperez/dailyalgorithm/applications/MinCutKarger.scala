package org.pfcoperez.dailyalgorithm.applications

object MinCutKarger {

  import org.pfcoperez.dailyalgorithm.numericmethods.random.impl.IntRandom
  import org.pfcoperez.dailyalgorithm.numericmethods.random.RandomGenerator
  import org.pfcoperez.dailyalgorithm.datastructures.graphs.undirected._

  def minCut[Node: Ordering, W](
    ug: UndirectedWeighedGraph[Node, W],
    randomSeed: Long): Option[Seq[Edge[Node, W]]] = {

    type EdgeType = Edge[Node, W]

    def merge(
      ug: UndirectedWeighedGraph[Node, W])(assimilated: Node, host: Node): UndirectedWeighedGraph[Node, W] =
      ((ug - assimilated) /: ug.edges(assimilated)) {
        case (updatedGraph, edge @ Edge(from, to, w)) =>
          Seq(from, to).find(!Set(assimilated, host).contains(_)) map { outNode =>
            updatedGraph + Edge(host, outNode, w)
          } getOrElse updatedGraph

      }

    @scala.annotation.tailrec
    def minCut(
      ug: UndirectedWeighedGraph[Node, W],
      randomGenerator: RandomGenerator[Int]): Option[Seq[Edge[Node, W]]] = {

      ug.nodes.size match {
        case 0 | 1 => None
        case 2 =>
          val Seq(a, b) = ug.nodes.toSeq
          Some(ug.edges)
        case _ =>
          val edges = ug.edges.toVector
          val (newGenerator, randomInteger) = randomGenerator.next
          val edge @ Edge(a, b, _) = edges(Math.abs(randomInteger) % edges.size)

          minCut(merge(ug)(a, b), newGenerator)
      }
    }

    minCut(ug, new IntRandom(randomSeed))

  }

}
