package org.pfcoperez.dailyalgorithm.datastructures.graphs.directed.trees

package object binary {

  trait BinaryTree[+T]
  case class Node[T](left: BinaryTree[T], v: T, right: BinaryTree[T]) extends BinaryTree[T]
  case object Empty extends BinaryTree[Nothing]

  trait BinaryTreeOps {
    def min[T: Ordering](btree: BinaryTree[T]): Option[T]
    def max[T: Ordering](btree: BinaryTree[T]): Option[T]
    def findFirst[T: Ordering](btree: BinaryTree[T])(v: T): BinaryTree[T]
    def insert[T: Ordering](btree: BinaryTree[T])(v: T): BinaryTree[T]
    def delete[T: Ordering](btree: BinaryTree[T])(v: T): BinaryTree[T]
    def height[T](binaryTree: BinaryTree[T], limit: Option[Int] = None): Int

    def toList[T: Ordering](btree: BinaryTree[T]): List[T]

    /**
     * Create a new binary tree by combining each node value
     * with its heigth in the tree.
     *
     * @param binaryTree Source binary tree
     * @return New binary tree containing the same values paired
     *         with each node height.
     */
    def zipWithHeight[T](binaryTree: BinaryTree[T]): BinaryTree[(T, Int)]

    def map[T, S](btree: BinaryTree[T])(f: T => S): BinaryTree[S]

    /**
     * Merge two binary trees. If they are balanced, the resulting tree
     * will be balanced too.
     *
     * @param a Binary tree
     * @param b Binary tree
     * @return Merged binary tree keeping balance if both input trees
     *         were balanced.
     */
    def merge[T](a: BinaryTree[T], b: BinaryTree[T])(implicit ord: Ordering[T]): BinaryTree[T]
  }

}
