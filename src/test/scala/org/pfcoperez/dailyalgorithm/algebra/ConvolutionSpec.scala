package org.pfcoperez.dailyalgorithm.algebra

import org.scalatest.{ Matchers, WordSpec }

import org.pfcoperez.dailyalgorithm.Algebra.Matrix._
import org.pfcoperez.dailyalgorithm.Algebra.Matrix

import NumericMatrix.Implicits._

class ConvolutionSpec extends WordSpec with Matchers {

  def identityKernel(f: Int): Matrix[Int] = {
    require(f % 2 == 1)
    positionalValues(f, f) { (i, j) =>
      if (i == f / 2 && i == j) 1 else 0
    }
  }

  import MatrixMultiplicationSpec.randomIntMatrix
  import DefaultConvolutionMethod.convolve

  "Matrix convolution" when {

    "used with identity kernels" should {

      "remove the outer layer of the input matrix if stride is 1" in {

        val (nM, mM) = (5, 5)
        val f = 3

        val m = randomIntMatrix(nM, nM)
        val k = identityKernel(f)

        val expected = positionalValues(nM - 2, mM - 2) { (i, j) =>
          m(i + 1)(j + 1)
        }

        println(PrintableMatrix(k))
        println
        println(PrintableMatrix(m))
        println
        println(PrintableMatrix(expected))
        println

        convolve(m, k) should equal(expected)

      }

      "provide the inner corners of the input matrix if stride is 2" in {

        val (nM, mM) = (5, 5)
        val f = 3

        val m = randomIntMatrix(nM, nM)
        val k = identityKernel(f)

        val expected = positionalValues(2, 2) { (i, j) =>
          m(1 + i * 2)(1 + j * 2)
        }

        println(PrintableMatrix(k))
        println
        println(PrintableMatrix(m))
        println
        println(PrintableMatrix(expected))
        println

        convolve(m, k, 2) should equal(expected)

      }

    }

    /*"used with border detection kernels" should {

      val verticalBorder = IndexedSeq.fill(3)(IndexedSeq(1, 0, -1))

      """detect "pixels" belonging to a vertical border""" in {

        val m = positionalValues(5,5) { (i, j) =>
          if(i < 4 && j < 4) 1 else 0
        }

        println(PrintableMatrix[Int](verticalBorder))
        println
        println(PrintableMatrix(m))
        println

        val result = convolve(m, verticalBorder)

        println(PrintableMatrix(result))
        println

      }

    }*/

  }

}
