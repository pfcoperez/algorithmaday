package org.pfcoperez.dailyalgorithm

import scala.collection.immutable.Queue

object Graphs {

  trait DirectedGraph[T]
  case class Node[T](value: T, children: List[Node[T]]) extends DirectedGraph[T]
  case class Empty[T]() extends DirectedGraph[T]

  /**
    * O(n), n = number of nodes
    */
  def breadthFirstTake[T, S](g: DirectedGraph[T])(f: PartialFunction[T, S]): List[S] = {

    def traverseRec(toVisit: Queue[DirectedGraph[T]], acc: List[S]): List[S] = toVisit.headOption collect {
      case Node(value, children) =>
        val (newToVisit, toAcc) = f.lift(value) map { res =>
          (toVisit.tail ++ children, Some(res))
        } getOrElse(toVisit.tail -> None)
        traverseRec(newToVisit, toAcc map (_::acc) getOrElse acc)
    } getOrElse acc.reverse

    traverseRec(Queue(g), Nil)

  }

  /**
    * O(n), n = number of nodes
    *
    * Use example:
    *
    * val g: DirectedGraph[Int] =
    *   Node(1, Node(2, Node(5, Nil)::Nil):: Node(3, Node(6, Node(7, Nil)::Nil)::Nil)::Node(4, Nil)::Nil)
    *
    * depthFirstTake(g) { case x => x }
    *
    */
  def depthFirstTake[T, S](g: DirectedGraph[T])(f: PartialFunction[T, S]): List[S] = {

    type Stack[T] = List[T]

    def traverseRec(toVisit: Stack[DirectedGraph[T]], acc: List[S]): List[S] = toVisit.headOption collect {
      case Node(value, children) =>
        val (newToVisit, toAcc) = f.lift(value) map { res =>
          (children ++ toVisit.tail, Some(res))
        } getOrElse(toVisit.tail -> None)
        traverseRec(newToVisit, toAcc map (_::acc) getOrElse acc)
    } getOrElse acc.reverse

    traverseRec(g::Nil, Nil)

  }

  /**
    * O(n)*T(f)
    */
  def breadthFirstFold[T,S](z: S, g: DirectedGraph[T])(f: PartialFunction[(S, T), S]): S = {

    def traverseRec(toVisit: Queue[DirectedGraph[T]], acc: S): S =
      toVisit.headOption collect {
        case Node(value, children) =>
          val (newToVisit, newAcc) = f.lift(acc, value) map { res =>
            (toVisit.tail ++ children, res)
          } getOrElse(toVisit.tail -> acc)
          traverseRec(newToVisit, newAcc)
      } getOrElse acc

    traverseRec(Queue(g), z)

  }

  
}
