package org.pfcoperez.dailyalgorithm.applications

import MinCutKarger._
import org.pfcoperez.dailyalgorithm.datastructures.graphs.undirected._

import org.scalatest.{ FlatSpec, Matchers }

class MinCutKargerSpec extends FlatSpec with Matchers {

  val graphA = {
    val nodes = (1 to 6).toSet

    val edges: Seq[Edge[Int, NoWeight]] = Seq(
      Edge(1, 2), Edge(2, 3), Edge(3, 1),
      Edge(3, 4),
      Edge(4, 5), Edge(5, 6), Edge(6, 4))

    val ug = (UndirectedGraph(nodes, Nil) /: edges) { (ug, edge) => ug + edge }

    println(ug.edges.toList)

    ug

  }

  def timeBasedSeed(): Long = System.currentTimeMillis()

  "MinCutKarger implementation" should "be able to find a cut" in {

    val res = minCut(graphA, timeBasedSeed())

    println(res.map(_.toList))

  }

}
