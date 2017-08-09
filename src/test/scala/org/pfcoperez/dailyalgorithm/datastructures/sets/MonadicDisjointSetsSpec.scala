package org.pfcoperez.dailyalgorithm.datastructures.sets

import org.scalatest.{ FlatSpec, Matchers }

import cats._

class MonadicDisjointSetsSpec extends FlatSpec with Matchers {

  "Operations at MonadicDisjointSets object" should "offer a monadic state interface for DisjointSets" in {

    import MonadicDisjointSets._

    /**
     *  Using the "for" notation to seamlessly handle
     *  state propagation after each operation.
     *
     */
    val operations = for {
      _ <- unionState(1, 2)
      oneAndTwo <- findState(2)
      _ <- unionState(3, 4)
      threeAndFour <- findState(3)
      _ <- unionState(2, 3)
      allFromOne <- findState(1)
      allFromTwo <- findState(2)
      allFromThree <- findState(3)
      allFromFour <- findState(4)
    } yield (oneAndTwo, threeAndFour, allFromOne, allFromTwo, allFromThree, allFromFour)

    /*
     val state0 = DisjointSets(1,2,3,4)
     val (state1, _) = state0.union(1,2)
     val (state2, oneAndTwo) = state1.find(2)
     val (state3, _) = state2.union(3,4)
     val (state4, threeAndFour) = state3.find(3)
     val (state5, _) = state4.union(2,3)
     val (state6, allFromOne) = state5.find(1)
     val (state7, allFromTwo) = state5.find(2)
     val (state8, allFromThree) = state5.find(3)
     val (state9, allFromFour) = state5.find(4)
     */

    val (
      Some(a),
      Some(b),
      Some(c),
      Some(d),
      Some(e),
      Some(f)
      ) = operations.runA(DisjointSets(1, 2, 3, 4)).value

    a should not equal (b)
    c shouldBe d
    d shouldBe e
    e shouldBe f

  }

}
