package org.pfcoperez.dailyalgorithm.datastructures.graphs.directed

import org.scalatest.{ FlatSpec, Matchers }

class AncestrySpec extends FlatSpec with Matchers {

  val F = Node("F")
  val E = Node("E")
  val D = Node("D")
  val C = Node("C")
  val B = Node("B", Seq(E, F))
  val A = Node("A", Seq(B, C, D))
  val H = Node("H", Seq(F))
  val I = Node("I", Seq(H))
  val G = Node("G", Seq(I))

  val nodes = Seq(A, B, C, D, E, F, G, H, I)

  "The findParents function" should "generate the whole ancestry of the provided nodes" in {

    val ancestry = findParents(nodes)

    ancestry.get(A) shouldBe None
    ancestry.get(B) shouldBe Some(Set(A))
    ancestry.get(C) shouldBe Some(Set(A))
    ancestry.get(D) shouldBe Some(Set(A))
    ancestry.get(E) shouldBe Some(Set(B))
    ancestry.get(F) shouldBe Some(Set(B, H))
    ancestry.get(G) shouldBe None
    ancestry.get(H) shouldBe Some(Set(I))
    ancestry.get(I) shouldBe Some(Set(G))

  }

}
