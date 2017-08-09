package org.pfcoperez.dailyalgorithm.applications

import scala.collection.immutable.Queue
import org.pfcoperez.dailyalgorithm.datastructures.graphs.directed.trees.binary._

object BFSAndApps extends App {

  /**
   * Level in-order traversal: First last level, from left to right,...
   * ... then second to last, from left to right, third to last ...
   * up to first element.
   *
   * O(n)
   *
   */
  def levelOrderTreeTraversal[T](tree: BinaryTree[T]): Seq[T] = {
    val res = bfsWithAccFunction(List.empty[T])(Queue(tree)) {
      (prev, level, v) =>
        v :: prev
    } { (a, b) => (b, a) }
    res
  }

  /**
   * Breadth-First traversal with accumulation function.
   * O(n), n = no elements
   */
  def bfsWithAccFunction[T, R](acc: R, h: Int = 0)(
    toVisit: Queue[BinaryTree[T]])(update: (R, Int, T) => R)(
    inLevelOrder: (BinaryTree[T], BinaryTree[T]) => (BinaryTree[T], BinaryTree[T])): R =
    if (toVisit.isEmpty) acc
    else {
      val (currentNode, remToVisit) = toVisit.dequeue
      val (newToVisit: Queue[BinaryTree[T]], newAcc) = currentNode match {
        case Node(left, v, right) =>
          val (a, b) = inLevelOrder(left, right)
          (remToVisit ++ Seq(a, b), update(acc, h, v))
        case _ => remToVisit -> acc
      }
      bfsWithAccFunction[T, R](newAcc, h + 1)(newToVisit)(update)(inLevelOrder)
    }

  val o = Node(Empty, 15, Empty)
  val n = Node(Empty, 14, Empty)
  val m = Node(Empty, 13, Empty)
  val l = Node(Empty, 12, Empty)
  val k = Node(Empty, 11, Empty)
  val j = Node(Empty, 10, Empty)
  val i = Node(Empty, 9, Empty)
  val h = Node(Empty, 8, Empty)

  val g = Node(n, 7, o)
  val f = Node(l, 6, m)
  val e = Node(j, 5, k)
  val d = Node(h, 4, i)

  val b = Node(d, 2, e)
  val c = Node(f, 3, g)

  val a = Node(b, 1, c)

  println(levelOrderTreeTraversal(a))

}
