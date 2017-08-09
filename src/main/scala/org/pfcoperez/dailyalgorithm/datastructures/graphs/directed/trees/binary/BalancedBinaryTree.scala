package org.pfcoperez.dailyalgorithm.datastructures.graphs.directed.trees.binary

object BalancedBinaryTree extends BinaryTreeOps {
  def min[T: Ordering](btree: BinaryTree[T]): Option[T] = RawBinaryTree.min(btree)
  def max[T: Ordering](btree: BinaryTree[T]): Option[T] = RawBinaryTree.max(btree)

  def findFirst[T: Ordering](btree: BinaryTree[T])(v: T): BinaryTree[T] =
    RawBinaryTree.findFirst(btree)(v)

  def height[T](binaryTree: BinaryTree[T], limit: Option[Int] = None): Int =
    RawBinaryTree.height(binaryTree, limit)

  private def leftRotation[T]: PartialFunction[Node[T], Node[T]] = {
    case Node(left, value, Node(rightsLeft, rightsValue, rightsRight)) =>
      Node(Node(left, value, rightsLeft), rightsValue, rightsRight)
    case other => other
  }

  private def rightRotation[T]: PartialFunction[Node[T], Node[T]] = {
    case Node(Node(leftsLeft, leftsValue, leftsRight), value, right) =>
      Node(leftsLeft, leftsValue, Node(leftsRight, value, right))
    case other => other
  }

  def insert[T](btree: BinaryTree[T])(v: T)(
    implicit
    order: Ordering[T]): BinaryTree[T] = {
    import order.mkOrderingOps

    def balancedInsert(btree: BinaryTree[T]): (BinaryTree[T], Option[Int]) = {

      def createBalancedSubtree(
        targetBranch: BinaryTree[T],
        secondBranch: BinaryTree[T],
        rotation: PartialFunction[Node[T], Node[T]])(
        nodeBuilder: (BinaryTree[T], BinaryTree[T]) => Node[T]): (BinaryTree[T], Option[Int]) = {
        val (updatedBranch, heightTrack) = balancedInsert(targetBranch)
        val h2propagate = heightTrack flatMap { h =>
          val othersh = height(secondBranch, Some(h + 2))
          if (h - othersh >= 1) None
          else Some(math.max(h, othersh) + 1)
        }
        val newNode = nodeBuilder(updatedBranch, secondBranch)
        (if (heightTrack.isEmpty != h2propagate.isEmpty) rotation(newNode) else newNode, h2propagate)

      }

      btree match {
        case Empty => Node(Empty, v, Empty) -> Some(0)
        case node @ Node(left, nodeval, right) =>
          if (nodeval == v) node -> Some(height(node))
          else if (v < nodeval)
            createBalancedSubtree(left, right, rightRotation) {
              Node(_, nodeval, _)
            }
          else
            createBalancedSubtree(right, left, leftRotation) {
              (targetBranch, secondBranch) => Node(secondBranch, nodeval, targetBranch)
            }
      }
    }

    balancedInsert(btree)._1

  }

  override def delete[T: Ordering](btree: BinaryTree[T])(v: T): BinaryTree[T] = ???

}
