package org.pfcoperez.dailyalgorithm.datastructures.graphs.directed

import org.scalatest.{ Matchers, WordSpec }
import org.scalatest.Inspectors._

class HigherOrderGraphFunctions extends WordSpec with Matchers {

  val G01: DirectedWeighedGraph[Char, Int] = DijkstraSpec.A // A Directed & weighted graph
  val dijkstraDistancesForG01 = DijkstraSpec.expectedDistances

  "DirectedGraph's map function" when {

    "passed a weight modification function" should {

      "create a new graphs with updated weights" in {
        val newGraph = map(G01)(identity, _ * 2)
        val newDijkstraDistances =
          minDistancesDijkstra(newGraph.asInstanceOf[Node[Char, Int]]) map {
            case (k, v) => k.value -> v
          }

        forAll(newDijkstraDistances) {
          case (dest, newDistance) =>
            dijkstraDistancesForG01(dest) * 2 shouldEqual newDistance
        }

      }

    }

  }

}
