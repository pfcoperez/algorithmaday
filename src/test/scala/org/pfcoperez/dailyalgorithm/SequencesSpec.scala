package org.pfcoperez.dailyalgorithm

import org.scalatest.{ FlatSpec, Matchers }

import Sequences._

class SequencesSpec extends FlatSpec with Matchers {

  "maxConsecutiveSequence" should "be able to provided the longest consecutive sequence" in {

    val examplesAndExpectations: Seq[(List[Int], List[Int])] = Seq(
      (1 to 4).toList -> (1 to 4).toList,
      List(2, 4, 1, 3) -> (1 to 4).toList,
      List(1, 10, 20, 21, 22) -> List(20, 21, 22),
      List(1, 2, 3, 20, 21, 22, 23, 24) -> List(20, 21, 22, 23, 24),
      List(7, 1, 0, 9, 4, 3, 8) -> List(7, 8, 9),
      (Nil, Nil))

    examplesAndExpectations foreach {
      case (input, expected) =>
        maxConsecutiveSequence(input) shouldBe (expected)
    }

  }

}
