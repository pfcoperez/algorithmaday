package org.pfcoperez.dailyalgorithm.applications

import MinCutKarger._
import org.pfcoperez.dailyalgorithm.datastructures.graphs.undirected._

import org.scalatest.{ WordSpec, Matchers }

class MinCutKargerSpec extends WordSpec with Matchers {

  def timeBasedSeed(): Long = System.currentTimeMillis()

  "MinCutKarger implementation" when {

    val graphA = {
      val nodes = (1 to 6).toSet

      val edges: Seq[Edge[Int, NoWeight]] = Seq(
        Edge(1, 2), Edge(2, 3), Edge(3, 1),
        Edge(3, 4),
        Edge(4, 5), Edge(5, 6), Edge(6, 4))

      (UndirectedGraph(nodes, Nil) /: edges) { (ug, edge) => ug + edge }
    }

    "used to try to find the minimum cut for a graph" should {

      (0 until 20) foreach { it =>
        s"provide a feasible cut at iteration $it" in {
          val res = minCut(graphA, timeBasedSeed())
          res.get.size should be <= 2
        }
      }

    }

  }

}
