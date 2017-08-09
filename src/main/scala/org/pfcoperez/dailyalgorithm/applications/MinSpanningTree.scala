package org.pfcoperez.dailyalgorithm.applications

import cats.data.State.{ pure, get }
import org.pfcoperez.dailyalgorithm.datastructures.sets.DisjointSets
import org.pfcoperez.dailyalgorithm.datastructures.sets.MonadicDisjointSets.{ unionState, _ }

object MinSpanningTree extends App {

  type Arc[K, W] = ((K, K), W)

  /**
   * Kruskal algorithm for the minimum spanning tree as a graph
   * described as a list of arcs.
   *
   * O(n*Log(n)) where n=Number of arcs.
   *
   * @param arcs Input graph as the list of its arcs.
   * @tparam K Node id type
   * @tparam W Weight type
   * @return Minimum spanning tree as the list of its arcs
   */
  def kruskalMinSpanningTree[K, W: Ordering](arcs: Seq[Arc[K, W]]): Seq[Arc[K, W]] = {

    // First, get the node set, use it to...
    val nodeKeys = (Set.empty[K] /: arcs) {
      case (acc, ((from, to), _)) => acc + from + to
    }

    // ... create an initial connected component disjoint set forest
    val initialCCs = DisjointSets(nodeKeys.toSeq: _*)

    // And start proving arcs, select minimum arcs not creating loops.
    val (_, selectedArcs) =
      ((initialCCs, Seq.empty[Arc[K, W]]) /: arcs.sortBy(_._2)) {

        case ((components, selectedArcs), arch @ ((from, to), _)) =>

          val queryComponents = for {
            fromComponent <- findState(from)
            toComponent <- findState(to)
            _ <- {
              for (a <- fromComponent; b <- toComponent) yield unionState(a, b)
            } getOrElse pure[DisjointSets[K], Boolean](false)
            st <- get
          } yield st -> {
            //If the arc joins to different CCs, include it in the result.
            if (fromComponent != toComponent) arch +: selectedArcs
            else selectedArcs
          }

          queryComponents.runA(components).value
      }

    selectedArcs

  }

  {
    val sampleGraph: Seq[Arc[Char, Int]] =
      Seq(
        ('A' -> 'B', 30),
        ('A' -> 'C', 10),
        ('A' -> 'D', 20),
        ('B' -> 'D', 1),
        ('D' -> 'C', 2),
        ('C' -> 'B', 3))

    val minSpanningTreeArcs = kruskalMinSpanningTree(sampleGraph)

    // It should print `13`
    println(s"Total weight: " + minSpanningTreeArcs.map(_._2).sum)
  }

}
