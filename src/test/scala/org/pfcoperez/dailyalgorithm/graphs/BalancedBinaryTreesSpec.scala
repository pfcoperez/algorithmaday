package org.pfcoperez.dailyalgorithm.graphs

import org.pfcoperez.dailyalgorithm.Graphs.BinaryTrees._
import org.scalatest.{FlatSpec, Matchers}

class BalancedBinaryTreesSpec extends FlatSpec with Matchers {

  import BalancedBinaryTree._

  def checkBalance[T](binaryTree: BinaryTree[T]): Unit = {

    def siblingsHeights[T](btree: BinaryTree[T]): List[(Int, Int)] = btree match {
      case Empty => (0,0)::Nil
      case Node(left, _, right) =>
        siblingsHeights(left) ++ List(height(left) -> height(right)) ++ siblingsHeights(right)
    }

    siblingsHeights(binaryTree) foreach { case (lh, rh) =>
      math.abs(lh-rh) should be <= 2
    }

  }

  "A Balanced Binary Tree" should "keep an uniform height at all its sub-trees" in {

    val input = 1 to 1000

    val btree: BinaryTree[Int] = ((Empty: BinaryTree[Int]) /: input) {
      (tree, element) =>
        val newTree = insert(tree)(element)
        checkBalance(newTree) // Balance is tested after each insert
        newTree
    }

    checkBalance(btree)

    toList(btree) shouldBe (input.toList.sorted)

  }

  it should "keep that balance not matter which side is to be more loaded" in {

    val input = 1000 to 1 by -1

    val btree: BinaryTree[Int] = ((Empty: BinaryTree[Int]) /: input) {
      (tree, element) =>
        val newTree = insert(tree)(element)
        checkBalance(newTree) // Balance is tested after each insert
        newTree
    }

    checkBalance(btree)

    toList(btree) shouldBe (input.toList.sorted)

  }

}
