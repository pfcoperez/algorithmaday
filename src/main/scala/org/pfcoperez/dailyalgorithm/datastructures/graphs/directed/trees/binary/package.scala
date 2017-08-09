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

    def toList[T: Ordering](btree: BinaryTree[T]): List[T] = {

      def toList(btree: BinaryTree[T])(acc: List[T]): List[T] = btree match {
        case Node(left, v, right) => toList(left)(v :: toList(right)(acc))
        case _ => acc
      }

      toList(btree)(Nil)

    }

    /**
     * Create a new binary tree by combining each node value
     * with its heigth in the tree.
     *
     * O(n), n=number of nodes
     *
     * @param binaryTree Source binary tree
     * @return New binary tree containing the same values paired
     *         with each node height.
     */
    def zipWithHeight[T](binaryTree: BinaryTree[T]): BinaryTree[(T, Int)] =
      binaryTree match {
        case Node(left, v, right) =>
          val newChildren @ Seq(newLeft, newRight) = Seq(left, right) map zipWithHeight
          val h = {
            newChildren map {
              case Empty => 1
              case Node(_, (_, h), _) => h + 1
            }
          }.max
          Node(newLeft, v -> h, newRight)
        case _ => Empty
      }

    def map[T, S](btree: BinaryTree[T])(f: T => S): BinaryTree[S] = btree match {
      case Node(left, v, right) => Node(map(left)(f), f(v), map(right)(f))
      case _ => Empty
    }

    /**
     * Merge two binary trees. If they are balanced, the resulting tree
     * will be balanced too.
     *
     * @param a Binary tree
     * @param b Binary tree
     * @return Merged binary tree keeping balance if both input trees
     *         were balanced.
     */
    def merge[T](a: BinaryTree[T], b: BinaryTree[T])(implicit ord: Ordering[T]): BinaryTree[T] = (a, b) match {

      case (Empty, _) => b
      case (_, Empty) => a
      case _ =>

        val sources @ Seq(la, lb) = Seq(a, b) map toList[T]

        val maxValue = (sources map (_.max)) max

        def consumeAndMerge(la: List[T], lb: List[T])(acc: BinaryTree[T]): BinaryTree[T] = {
          Seq(la, lb).sortBy(_.headOption.getOrElse(maxValue)) match {
            case Seq(av :: arem, b) =>
              consumeAndMerge(arem, b)(insert(acc)(av))
            case _ => acc
          }
        }

        consumeAndMerge(la, lb)(Empty)

    }

  }

}
