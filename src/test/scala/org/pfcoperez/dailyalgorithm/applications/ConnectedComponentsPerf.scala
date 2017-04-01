package org.pfcoperez.algorithmaday.applications

import org.scalameter.api._

object ConnectedComponentsBenchmark extends Bench.LocalTime {

  def generateForest(nNodes: Int, nArchs: Int): (Set[Int], Seq[(Int, Int)]) = {
    import org.scalacheck.Gen
    val nodes = Seq(1 to nNodes: _*)
    val archGenerator: Gen[(Int, Int)] =
      for {
        from <- Gen.oneOf(nodes)
        to <- Gen.oneOf(nodes.filterNot(_ == from))
      } yield (from, to)
    nodes.toSet -> Gen.listOfN(nArchs, archGenerator).sample.get
  }

  val graphGenerator = for {
    nNodes <- Gen.range("No.Nodes")(1, 10000, 250)
    archsRatio <- Gen.range("Archs Ratio")(0, 240, 20)
  } yield generateForest(nNodes, (nNodes*archsRatio/100.0).toInt)

  val caseGenerator = for {
    graph <- graphGenerator
    implementation <- Gen.enumeration("Implementation")(
      RegularSetsConnectedComponentsOps,
      DisjointSetsConnectedComponentsOps
    )
  } yield (implementation, graph)

  performance of "Connected Components" in {
    measure method "connectedNodes" in {
      using(caseGenerator) curve("Implementation") in { case (implementation, (nodes, archs)) =>
        implementation.connectedNodes(nodes, archs)
      }
    }
  }


}
