package org.pfcoperez.dailyalgorithm.datastructures.graphs.directed.trees.binary

import org.scalatest.{ FlatSpec, Matchers }

class BalancedBinaryTreeSpec extends FlatSpec with Matchers {

  import BalancedBinaryTree._

  def generateBalancedTree[T: Ordering](
    input: Seq[T],
    checkBalanceAtEachInsert: Boolean = false): BinaryTree[T] =
    ((Empty: BinaryTree[T]) /: input) {
      (tree, element) =>
        val newTree = insert(tree)(element)
        if (checkBalanceAtEachInsert)
          checkBalance(newTree) // Balance is tested after each insert
        newTree
    }

  def checkBinaryTreeInvariant[T](btree: BinaryTree[T])(implicit ord: Ordering[T]): Unit = {
    import ord.mkOrderingOps

    def checkInvariant(btree: BinaryTree[T])(invariant: T => Boolean): Boolean = btree match {
      case Node(left, v, right) =>
        invariant(v) && checkInvariant(left)(_ <= v) && checkInvariant(right)(_ > v)
      case _ => true
    }

    checkInvariant(btree)(_ => true) shouldBe true

  }

  def checkBalance[T](binaryTree: BinaryTree[T]): Unit = {

    def siblingsHeights[T](btree: BinaryTree[T]): List[(Int, Int)] = btree match {
      case Empty => (0, 0) :: Nil
      case Node(left, _, right) =>
        siblingsHeights(left) ++ List(height(left) -> height(right)) ++ siblingsHeights(right)
    }

    siblingsHeights(binaryTree) foreach {
      case (lh, rh) =>
        math.abs(lh - rh) should be <= 1
    }

  }

  "A Balanced Binary Tree" should "keep an uniform height at all its sub-trees" in {

    val input = 1 to 1000

    val btree = generateBalancedTree(input, true)

    checkBalance(btree)

    checkBinaryTreeInvariant(btree)

    toList(btree) shouldBe (input.toList.sorted)

  }

  it should "keep that balance not matter which side is to be more loaded" in {

    val input = 1000 to 1 by -1

    val btree = generateBalancedTree(input, true)

    checkBalance(btree)

    checkBinaryTreeInvariant(btree)

    toList(btree) shouldBe (input.toList.sorted)

  }

  it should "keep balance when merge with another balanced binary tree" in {

    val a = generateBalancedTree(1 to 72)
    val b = generateBalancedTree(123 to 73 by -1)

    checkBalance(a)
    checkBalance(b)

    val possibleBlends = Seq(merge(a, b), merge(b, a))

    possibleBlends foreach { combinedBtree =>
      checkBinaryTreeInvariant(combinedBtree)
      checkBalance(combinedBtree)
      toList(combinedBtree) should contain theSameElementsAs (toList(a) ++ toList(b))
    }

  }

}
