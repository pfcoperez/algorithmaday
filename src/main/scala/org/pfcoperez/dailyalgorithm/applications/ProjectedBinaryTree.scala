package org.pfcoperez.dailyalgorithm.applications

import org.pfcoperez.dailyalgorithm.datastructures.graphs.directed.trees.binary._

object ProjectedBinaryTree extends App {

  /**
   * Builds an sparse 2D matrix of the projection of a binary tree into
   * a 2D plane using an uniform distance between a node and its children.
   *
   * O(n), n = Number of nodes.
   *
   */
  def projectBinaryTree[T](btree: BinaryTree[T]): Map[(Int, Int), Seq[T]] = {

    def projection(
      currentNode: BinaryTree[T],
      currentPosition: (Int, Int),
      acc: Map[(Int, Int), Seq[T]] = Map.empty): Map[(Int, Int), Seq[T]] = currentNode match {

      case Empty => acc
      case Node(left, v, right) =>
        val (i, j) = currentPosition

        val fromLeft = projection(left, (i + 1, j - 1), acc)

        val withCurrent = fromLeft + {
          currentPosition -> (v +: (acc get currentPosition getOrElse Seq()))
        }

        projection(right, (i + 1, j + 1), withCurrent) // From right sub-tree

    }

    val projectionWidth = RawBinaryTree.height(btree) + 1

    projection(btree, (0, projectionWidth / 2)).mapValues(_.reverse)

  }

  // A sample

  val example = ((Empty: BinaryTree[Int]) /: (1 to 20)) {
    (tree, element) => BalancedBinaryTree.insert(tree)(element)
  }

  val exampleProjection = projectBinaryTree(example)

  val (maxi, maxj) = exampleProjection.keys.max

  def boxedString[T](x: T, l: Int = 0): String =
    x.toString.reverse.zipAll(1 to l, ' ', 0).reverse.map(_._1).mkString

  val l = 5
  for (i <- 0 to maxi) {
    for (j <- 0 to maxj) {
      print(
        boxedString(
          exampleProjection get (i -> j) map (v =>
            boxedString(v.mkString(","), l)) getOrElse " " * l))
    }
    println
  }

}
