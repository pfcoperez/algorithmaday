package org.pfcoperez.dailyalgorithm.applications

import org.scalacheck.Gen
import org.scalatest.{ FlatSpec, Matchers }

class ConnectedComponentsSpec extends FlatSpec with Matchers {

  def generateForest(nNodes: Int, nArchs: Int): (Set[Int], Seq[(Int, Int)]) = {
    val nodes = Seq(1 to nNodes: _*)
    val archGenerator: Gen[(Int, Int)] =
      for {
        from <- Gen.oneOf(nodes)
        to <- Gen.oneOf(nodes.filterNot(_ == from))
      } yield (from, to)
    nodes.toSet -> Gen.listOfN(nArchs, archGenerator).sample.get
  }

  "Both implementations (Deep First Search & Disjoint Sets)" should "provide equivalent results" in {

    val nNodes: Int = 1000
    val archsRatio: Double = 0.8
    val nArchs: Int = (nNodes * archsRatio) toInt

    val (nodes, archs) = generateForest(nNodes, nArchs)

    val traditionalCCs = RegularSetsConnectedComponentsOps.connectedNodes(nodes, archs).toSet
    val disjointsetsCCs = DisjointSetsConnectedComponentsOps.connectedNodes(nodes, archs).toSet

    traditionalCCs should equal(disjointsetsCCs)

  }

}
