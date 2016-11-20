package org.pfcoperez.dailyalgorithm.graphs

import org.pfcoperez.dailyalgorithm.Graphs.BinaryTrees._

import org.scalatest._


class BinaryTreesSpec extends FlatSpec with Matchers {

  import RawBinaryTree._

  val input = Seq(5,6,0,3,2,1)

  val btree: BinaryTree[Int] = ((Empty: BinaryTree[Int]) /: input) {
    (tree, element) => insert(tree)(element)
  }


  "BinaryTrees" should "be able to find the elements on them inserted" in {

    toList(btree) should be (input.sorted)

  }

  it should "provide the right minimum and maximum values" in {

    min(btree) shouldBe Some(0)
    max(btree) shouldBe Some(6)

  }

}
