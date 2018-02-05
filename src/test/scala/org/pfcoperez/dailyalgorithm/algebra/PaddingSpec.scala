package org.pfcoperez.dailyalgorithm.algebra

import org.pfcoperez.dailyalgorithm.Algebra.Matrix._
import org.pfcoperez.dailyalgorithm.Algebra.Matrix.NumericMatrix

import org.scalatest.{ FlatSpec, Matchers }

class PaddingSpec extends FlatSpec with Matchers {

  "A padded numeric matrix" should "provide zero values for the additional positions" in {

    val padding = 2
    val (n, m) = (13, 23)

    val matrix = positionalValues(n, m) {
      case (i, j) =>
        i * m + j + 1
    }

    val paddedMatrix = matrix.padded(padding)

    def isPaddedPosition(i: Int, j: Int): Boolean =
      i < padding || i >= n || j < padding || j >= m

    for {
      i <- 0 until (n + padding)
      j <- 0 until (m + padding)
    } {
      val value = paddedMatrix(i)(j)

      if (isPaddedPosition(i, j)) value shouldBe 0
      else value should not be 0
    }

  }

}