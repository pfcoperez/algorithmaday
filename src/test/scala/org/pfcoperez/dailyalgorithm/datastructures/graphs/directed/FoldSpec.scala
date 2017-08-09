package org.pfcoperez.dailyalgorithm.datastructures.graphs.directed

import org.scalatest.{ FlatSpec, Matchers }

class FoldSpec extends FlatSpec with Matchers {

  "breadthFirstFold" should "allow to be used to implement breadthFirstTake" in {

    def breadthFirstTakeUsingFold[T, S](g: DirectedGraph[T])(f: PartialFunction[T, S]): List[S] =
      breadthFirstFold(List.empty[S], g) {
        case (acc: List[S], v: T @unchecked) if f.isDefinedAt(v) => f(v) :: acc
      } reverse

    val pf: PartialFunction[Int, Int] = {
      case v if v != 6 => v
    }

    val g: DirectedGraph[Int] =
      Node(1, Node(2, Node(5, Nil) :: Nil) :: Node(3, Node(6, Node(7, Nil) :: Nil) :: Nil) :: Node(4, Nil) :: Nil)

    breadthFirstTakeUsingFold(g)(pf) should contain theSameElementsInOrderAs breadthFirstTake(g)(pf)

  }

}
