package org.pfcoperez.dailyalgorithm.database

import org.scalatest.{ FlatSpec, Matchers }
import org.pfcoperez.dailyalgorithm.Databases._

class JoinOperationsSpec extends FlatSpec with Matchers {

  val ma = Map(0 -> " ", 1 -> "a", 2 -> "b")
  val mb = Map(1 -> "A", 2 -> "B", 3 -> "C")

  "An map with extended db operations" should "be able to perform inner joins" in {

    (ma joinInner mb) shouldBe Map(1 -> ("a", "A"), 2 -> ("b", "B"))

  }

  it should "be able to perform left outer joins" in {

    val expected = Map(
      0 -> (" ", None),
      1 -> ("a", Some("A")),
      2 -> ("b", Some("B")))

    (ma joinOuterLeft mb) shouldBe expected

  }

  it should "be able to perform right outer joins" in {

    val expected = Map(
      1 -> (Some("a"), "A"),
      2 -> (Some("b"), "B"),
      3 -> (None, "C"))

    (ma joinOuterRight mb) shouldBe expected

  }

}
