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
      **/
    val operations = for {
      _ <- unionState(1,2)
      oneAndTwo <- findState(2)
      _ <- unionState(3,4)
      threeAndFour <- findState(3)
      _ <- unionState(2,3)
      allFromOne <- findState(1)
      allFromTwo <- findState(2)
      allFromThree <- findState(3)
      allFromFour <- findState(4)
    } yield (
      oneAndTwo,
      threeAndFour,
      allFromOne,
      allFromTwo,
      allFromThree,
      allFromFour
    )

    val (
      Some(a),
      Some(b),
      Some(c),
      Some(d),
      Some(e),
      Some(f)
    ) = operations.runA(DisjointSets(1,2,3,4)).value

    a should not equal (b)
    c shouldBe d
    d shouldBe e
    e shouldBe f

  }

}
