package org.pfcoperez.dailyalgorithm.datastructures.graphs.directed

import org.scalatest.{ FlatSpec, Matchers }

class BidirectionalSearchSpec extends FlatSpec with Matchers {

  "Bidirectional search" should "be able to find whther any two nodes are connected" in {

    val F = Node("F")
    val E = Node("E")
    val D = Node("D")
    val C = Node("C")
    val B = Node("B", Seq(E, F))
    val A = Node("A", Seq(B, C, D))
    val H = Node("H", Seq(F))
    val I = Node("I", Seq(H))
    val G = Node("G", Seq(I))

    val pairsInTouch = Seq(
      A -> I,
      B -> H,
      E -> E)

    val unreachablePairs = Seq(
      E -> F,
      D -> C,
      D -> I)

    pairsInTouch foreach {
      case (from, to) =>
        atSameConnectedComponent(from, to) shouldBe true
        atSameConnectedComponent(to, from) shouldBe true
    }

    unreachablePairs foreach {
      case (from, to) =>
        println(from.value, to.value)
        atSameConnectedComponent(from, to) shouldBe false
        atSameConnectedComponent(to, from) shouldBe false
    }

  }

}
