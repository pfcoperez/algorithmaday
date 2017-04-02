package org.pfcoperez.algorithmaday.applications

import org.scalameter.api._
import org.scalameter.picklers.noPickler._

object ConnectedComponentsBenchmark extends Bench.LocalTime {
  
  type Graph = (Set[Int], Seq[(Int, Int)])

  def generateForest(nNodes: Int, nArchs: Int): Graph = {
    import org.scalacheck.Gen
    val nodes = Seq(1 to nNodes: _*)
    val archGenerator: Gen[(Int, Int)] =
      for {
        from <- Gen.oneOf(nodes)
        to <- Gen.oneOf(nodes/*.filterNot(_ == from)*/)
      } yield (from, to)
    nodes.toSet -> Gen.listOfN(nArchs, archGenerator).sample.get
  }

  val graphGenerator: Gen[Graph] = for {
    nNodes <- Gen.range("No.Nodes")(1, 1000, 250) //(1, 10000, 250)
    archsRatio <- Gen.range("Archs Ratio")(0, 300, 100) //(0, 240, 20)
  } yield generateForest(nNodes, (nNodes*archsRatio/100.0).toInt)

  val caseGenerator: Gen[(ConnectedComponentsOps, Graph)] = for {
    graph <- graphGenerator
    implementation <- Gen.enumeration("Implementation")(
      RegularSetsConnectedComponentsOps,
      DisjointSetsConnectedComponentsOps
    )
  } yield (implementation, graph)

  performance of "Connected Components" in {
    measure method "connectedNodes" in {
      using(caseGenerator) curve("Implementation") in {
        case (implementation: ConnectedComponentsOps, (nodes, archs)) =>
          implementation.connectedNodes(nodes, archs)
      }
    }
  }


}
