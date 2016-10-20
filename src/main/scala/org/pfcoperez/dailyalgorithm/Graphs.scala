package org.pfcoperez.dailyalgorithm

import scala.collection.immutable.Queue

object Graphs {

  trait DirectedGraph[T]
  case class Node[T](value: T, children: List[Node[T]]) extends DirectedGraph[T]
  case class Empty[T]() extends DirectedGraph[T]

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

}
