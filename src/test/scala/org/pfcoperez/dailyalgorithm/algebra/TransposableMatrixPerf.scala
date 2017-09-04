package org.pfcoperez.dailyalgorithm.algebra

import org.pfcoperez.dailyalgorithm.Algebra.Matrix._
import org.pfcoperez.dailyalgorithm.Algebra.Matrix

import org.scalameter.api._

import MatrixMultiplicationSpec.randomIntMatrix

object TransposableMatrixBenchmark extends Bench.OfflineReport {

  val maxNoElements = 400000

  def validateTranposition(checkValues: Boolean)(M: Matrix[Int], MTranspose: Matrix[Int]): Unit = {
    val (n, m) = size(M)
    val (nT, mT) = size(MTranspose)
    assert(n == mT && m == nT, "Dimensions must be swapped")
    if (checkValues) for (i <- 0 until n; j <- 0 until m)
      assert(M(i)(j) == MTranspose(j)(i), "Values at swapped positions have to match")
  }

  val matrixGenerator: Gen[Matrix[Int]] = for {
    s <- Gen.exponential("number of elements")(1, maxNoElements, 4)
    l = math.sqrt(s).toInt
  } yield randomIntMatrix(l, l)

  performance of "Matrix tranposition" in {

    measure method "Transpose" in {

      val validateValues = false

      using(matrixGenerator) curve "Daily-algorithm transpose (T)" in {
        m: Matrix[Int] => validateTranposition(validateValues)(m, m.T)
      }

      using(matrixGenerator) curve "Scala's collections transpose" in {
        m: Matrix[Int] => validateTranposition(validateValues)(m, m.transpose)
      }

    }

    measure method "Transpose & values check" in {

      val validateValues = true

      using(matrixGenerator) curve "Daily-algorithm transpose (T)" in {
        m: Matrix[Int] => validateTranposition(validateValues)(m, m.T)
      }

      using(matrixGenerator) curve "Scala's collections transpose" in {
        m: Matrix[Int] => validateTranposition(validateValues)(m, m.transpose)
      }

    }

  }

}
