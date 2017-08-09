package org.pfcoperez.dailyalgorithm.combinatorics

import org.scalatest.prop.Checkers
import org.scalatest.{ FlatSpec, Matchers }

class CartesianProductSpec extends FlatSpec with Checkers {

  import org.pfcoperez.dailyalgorithm.Combinatorics.{ cartesianProduct, improvedCartesianProductWithCats }

  import org.scalacheck.Gen
  import org.scalacheck.Prop.forAll

  "Both implementations of cartesian product using cats" should "provide the same results" in {

    val casesGenerator = Gen.listOfN(5, Gen.listOfN(10, Gen.posNum[Int]))

    check {
      forAll(casesGenerator) { collections =>
        improvedCartesianProductWithCats(collections: _*).toSet ==
          cartesianProduct(collections).toSet
      }
    }

  }

}
