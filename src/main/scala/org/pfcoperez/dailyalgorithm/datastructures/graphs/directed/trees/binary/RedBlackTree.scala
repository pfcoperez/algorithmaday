package org.pfcoperez.dailyalgorithm.datastructures.graphs.directed.trees.binary

import cats.{ Eval, Later, Now }
import cats.Eval.{ later, now }

trait RedBlackTreeOps extends BalancedBinaryTreeOps {

  sealed trait RedBlackTree[+T] extends BinaryTree[T] {
    def isRed: Boolean = false
    def left: Eval[BinaryTree[T]]
    def right: Eval[BinaryTree[T]]
  }

  case class BlackRoot[T](left: Eval[BinaryTree[T]], value: T, right: Eval[BinaryTree[T]]) extends RedBlackTree[T]

  case class RedBlackNode[+T](left: Eval[BinaryTree[T]], value: T, override val isRed: Boolean, right: Eval[BinaryTree[T]])(
    val parent: Eval[RedBlackTree[T]]) extends RedBlackTree[T]

  object BlackEmpty extends BinaryTree[Nothing]

  def leftRotation[T](node: RedBlackNode[T])(parent: Eval[RedBlackTree[T]]): Eval[BinaryTree[T]] = {
    node.right.flatMap {
      case prevRight: RedBlackNode[T] =>
        lazy val replacement: Eval[RedBlackNode[T]] = later {
          prevRight.copy(left = newLeft, right = prevRight.right)(parent)
        }
        lazy val newLeft = later {
          node.copy(right = prevRight.left)(replacement)
        }
        updateParent(replacement)(parent)
    }
  }

  def rightRotation[T](node: RedBlackNode[T])(parent: Eval[RedBlackTree[T]]): Eval[BinaryTree[T]] = {
    node.left.flatMap {
      case prevLeft: RedBlackNode[T] =>
        lazy val replacement: Eval[RedBlackNode[T]] = later {
          prevLeft.copy(left = prevLeft.left, right = newRight)(parent)
        }
        lazy val newRight = later {
          node.copy(left = prevLeft.right)(replacement)
        }
        updateParent(replacement)(parent)
    }
  }

  def updateParent[T](tree: Eval[BinaryTree[T]])(parent: Eval[RedBlackTree[T]]): Eval[BinaryTree[T]] = tree flatMap {
    case RedBlackNode(left, v, color, right) =>
      lazy val newTree: Eval[RedBlackNode[T]] = Later(RedBlackNode(updateParent(left)(newTree), v, color, updateParent(right)(newTree))(parent))
      newTree
    case other =>
      Now(other)
  }

  override def insert[T](btree: BinaryTree[T])(v: T)(implicit order: Ordering[T]): BinaryTree[T] = {
    import order.mkOrderingOps

    def insert(btree: BinaryTree[T])(v: T, parent: Eval[RedBlackTree[T]])(implicit order: Ordering[T]): BinaryTree[T] = {
      val unchecked = btree match {
        case BlackEmpty =>
          Now(RedBlackNode(Now(BlackEmpty), v, false, Now(BlackEmpty))(parent))
        case root @ BlackRoot(left, value, right) =>
          lazy val newRoot: Eval[RedBlackTree[T]] = later {
            if (v <= value) BlackRoot(left.map(insert(_)(v, newRoot)), value, updateParent(right)(newRoot))
            else BlackRoot(updateParent(left)(newRoot), value, right.map(insert(_)(v, newRoot)))
          }
          newRoot
        case node @ RedBlackNode(Now(left), value, isRed, Now(right)) if left == BlackEmpty && right == BlackEmpty =>
          lazy val replacement: Eval[RedBlackTree[T]] = later {
            if (v <= value) RedBlackNode(later(newNode), value, isRed, now(BlackEmpty))(node.parent)
            else RedBlackNode(now(BlackEmpty), value, isRed, later(newNode))(node.parent)
          }
          lazy val newNode: RedBlackNode[T] = RedBlackNode(now(BlackEmpty), v, isRed = false, now(BlackEmpty))(replacement)
          replacement
        case node @ RedBlackNode(left, value, _, right) =>
          lazy val replacement: Eval[RedBlackNode[T]] = later {
            if (v <= value) node.copy(left = left.map(insert(_)(v, replacement)))(node.parent)
            else node.copy(right = right.map(insert(_)(v, replacement)))(node.parent)
          }
          updateParent(replacement)(parent)
      }
      unchecked.value //TODO: Check balance
    }

    btree match {
      case btree: RedBlackNode[T] =>
        insert(btree)(v, btree.parent)
      case _: BlackRoot[T] =>
        insert(btree)(v, Later(???))
    }
  }

}

object RedBlackTree extends RedBlackTreeOps