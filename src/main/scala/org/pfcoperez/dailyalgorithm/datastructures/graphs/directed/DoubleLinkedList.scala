package org.pfcoperez.dailyalgorithm.datastructures.graphs.directed

trait DoubleLinkedList[+T]

object DoubleLinkedList {

  class Node[T](lazyPrev: => DoubleLinkedList[T], val value: T, lazyNext: => DoubleLinkedList[T]) extends DoubleLinkedList[T] {
    self =>

    lazy val prev: DoubleLinkedList[T] = lazyPrev
    lazy val next: DoubleLinkedList[T] = lazyNext

    def copy(lazyPrev: => DoubleLinkedList[T] = prev, value: T = self.value, lazyNext: => DoubleLinkedList[T] = next): Node[T] =
      new Node(lazyPrev, value, lazyNext)
  }

  object Node {
    def apply[T](lazyPrev: => DoubleLinkedList[T], value: T, lazyNext: => DoubleLinkedList[T]): Node[T] =
      new Node(lazyPrev, value, lazyNext)
    def unapply[T](node: Node[T]): Option[(DoubleLinkedList[T], T, DoubleLinkedList[T])] = Some(
      (node.prev, node.value, node.next)
    )
  }

  case object Empty extends DoubleLinkedList[Nothing]

  private def updatePrev[T](dl: DoubleLinkedList[T], prev: => DoubleLinkedList[T]): DoubleLinkedList[T] = dl match {
    case node: Node[T] =>
      lazy val updated = node.copy(lazyPrev = prev, lazyNext = newTail)
      lazy val newTail: DoubleLinkedList[T] = updatePrev(node.next, updated)
      updated
    case _ => Empty
  }

  /**
    * Prepend an element to a given double linked list.
    * O(n), n = list size
    */
  def prepend[T](dl: DoubleLinkedList[T], x: T, prev: DoubleLinkedList[T] = Empty): DoubleLinkedList[T] = {
    lazy val newHead: DoubleLinkedList[T] = Node(Empty, x, tail)
    lazy val tail = updatePrev(dl, newHead)
    newHead
  }

  /**
    * Append an element to a given double linked list.
    * O(n), n = list size
    */
  def append[T](dl: DoubleLinkedList[T], x: T, prev: DoubleLinkedList[T] = Empty): Node[T] = dl match {
    case Empty => Node(prev, x, Empty)
    case Node(nodePrev, value, nodeNext) =>
      lazy val updated: Node[T] = Node(prev, value, newNext)
      lazy val newNext = append(nodeNext, x, updated)
      updated
  }

  /**
    * Return the first element in the list, if present.
    * O(1)
    */
  def headOption[T](dl: DoubleLinkedList[T]): Option[T] = dl match {
    case Node(_, value, _) => Some(value)
    case _ => None
  }

  /**
    * Return the remaining list after removing the first element
    * if the list is not empty, [[Empty]] otherwise.
    * O(n), n = list size
    */
  def tail[T](dl: DoubleLinkedList[T]): DoubleLinkedList[T] = dl match {
    case Node(_, _, next) => updatePrev(next, Empty)
    case _ => Empty
  }

  def lastNode[T](dl: DoubleLinkedList[T]): DoubleLinkedList[T] = dl match {
    case Empty => Empty
    case node @ Node(_, _, Empty) => node
    case Node(_, _, next) => lastNode(next)
  }

  object syntax {
    implicit class DoubleLinkedListOps[T](dl: DoubleLinkedList[T]) {
      def +:(x: T): DoubleLinkedList[T] = prepend(dl, x)
      def :+(x: T): DoubleLinkedList[T] = append(dl, x)
      def headOption: Option[T] = DoubleLinkedList.headOption(dl)
      def tail: DoubleLinkedList[T] = DoubleLinkedList.tail(dl)
      def left: DoubleLinkedList[T] = dl match {
        case Empty => Empty
        case Node(left, _, _) => left
      }
      def right: DoubleLinkedList[T] = dl match {
        case Empty => Empty
        case Node(_, _, right) => right
      }
    }
  }

}

