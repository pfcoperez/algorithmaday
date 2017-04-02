package org.pfcoperez.algorithmaday.applications

import org.scalameter.api._

object ConnectedComponentsBenchmark extends Bench.OfflineRegressionReport {

  type Graph = (Set[Int], Seq[(Int, Int)])

  def generateForest(nNodes: Int, nArchs: Int): Graph = {
    import org.scalacheck.Gen
    val nodes = Seq(1 to nNodes: _*)
    nodes.toSet -> {
      if(nodes.nonEmpty) {
        val archGenerator: Gen[(Int, Int)] =
          for {
            from <- Gen.oneOf(nodes)
            to <- Gen.oneOf(nodes/*.filterNot(_ == from)*/)
          } yield (from, to)
        Gen.listOfN(nArchs, archGenerator).sample.get
      } else Seq.empty
    }
  }

  val graphGenerator: Gen[Graph] = for {
    nNodes <- Gen.range("No.Nodes")(0, 10000, 1000)
    archsRatio <- Gen.range("Archs Ratio")(0, 120, 20)
  } yield generateForest(nNodes, (nNodes*archsRatio/100.0).toInt)


  performance of "Connected Components" in {
    measure method "connectedNodes" in {
      using(graphGenerator) curve("Regular Sets") in {
        case (nodes, archs) =>
          RegularSetsConnectedComponentsOps.connectedNodes(nodes, archs)
      }
      using(graphGenerator) curve("Disjoint Sets") in {
        case (nodes, archs) =>
          DisjointSetsConnectedComponentsOps.connectedNodes(nodes, archs)
      }
    }
  }


}
