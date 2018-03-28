package org.pfcoperez.dailyalgorithm.datastructures.graphs.directed

import cats.Eval
import cats.Eval.{later, now}

trait DoubleLinkedListCats[+T]

object DoubleLinkedListCats {
  case class Node[T](prev: Eval[DoubleLinkedListCats[T]], value: T, next: Eval[DoubleLinkedListCats[T]]) extends DoubleLinkedListCats[T]

  case object Empty extends DoubleLinkedListCats[Nothing]

  private def updatePrev[T](dl: DoubleLinkedListCats[T], prev: Eval[DoubleLinkedListCats[T]]): DoubleLinkedListCats[T] = dl match {
    case node: Node[T] =>
      lazy val updated: Eval[DoubleLinkedListCats[T]] = later(node.copy(prev = prev, next = newTail))
      lazy val newTail = node.next.map(updatePrev(_, updated))
      updated.value
    case _ => Empty
  }

  /**
    * Prepend an element to a given double linked list.
    * O(n), n = list size
    */
  def prepend[T](dl: DoubleLinkedListCats[T], x: T, prev: DoubleLinkedListCats[T] = Empty): DoubleLinkedListCats[T] = {
    lazy val newHead: Eval[DoubleLinkedListCats[T]] = later(Node(now(Empty), x, tail))
    lazy val tail = later(updatePrev(dl, newHead))
    newHead.value
  }

  /**
    * Append an element to a given double linked list.
    * O(n), n = list size
    */
  def append[T](dl: DoubleLinkedListCats[T], x: T, prev: Eval[DoubleLinkedListCats[T]] = now(Empty)): Node[T] = dl match {
    case Empty => Node(prev, x, now(Empty))
    case Node(_, value, nodeNext) =>
      lazy val updated: Eval[Node[T]] = later(Node(prev, value, newNext))
      lazy val newNext = later(append(nodeNext.value, x, updated))
      updated.value
  }

  /**
    * Return the first element in the list, if present.
    * O(1)
    */
  def headOption[T](dl: DoubleLinkedListCats[T]): Option[T] = dl match {
    case Node(_, value, _) => Some(value)
    case _ => None
  }

  /**
    * Return the remaining list after removing the first element
    * if the list is not empty, [[Empty]] otherwise.
    * O(n), n = list size
    */
  def tail[T](dl: DoubleLinkedListCats[T]): DoubleLinkedListCats[T] = dl match {
    case Node(_, _, next) => updatePrev(next.value, now(Empty))
    case _ => Empty
  }

  def lastNode[T](dl: DoubleLinkedListCats[T]): DoubleLinkedListCats[T] = dl match {
    case Empty => Empty
    case node @ Node(_, _, next) if next.value == Empty => node
    case Node(_, _, next) => lastNode(next.value)
  }

  object syntax {
    implicit class DoubleLinkedListOps[T](dl: DoubleLinkedListCats[T]) {
      def +:(x: T): DoubleLinkedListCats[T] = prepend(dl, x)
      def :+(x: T): DoubleLinkedListCats[T] = append(dl, x)
      def headOption: Option[T] = DoubleLinkedListCats.headOption(dl)
      def tail: DoubleLinkedListCats[T] = DoubleLinkedListCats.tail(dl)
      def left: DoubleLinkedListCats[T] = dl match {
        case Empty => Empty
        case Node(left, _, _) => left.value
      }
      def right: DoubleLinkedListCats[T] = dl match {
        case Empty => Empty
        case Node(_, _, right) => right.value
      }
    }
  }
}


