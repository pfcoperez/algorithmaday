package org.pfcoperez.dailyalgorithm.algebra

import org.pfcoperez.dailyalgorithm.Algebra.Matrix._
import org.pfcoperez.dailyalgorithm.Algebra.Matrix
import org.pfcoperez.dailyalgorithm.Algebra.Matrix.{ size => shape }

import org.scalatest.{ Matchers, FlatSpec }

import MatrixMultiplicationSpec.randomIntMatrix

class TransposableMatrixSpec extends FlatSpec with Matchers {

  "A `TransposableMatrix` implicit class" should "seamlessly provided matrix transpose operation" in {

    val n = 50
    val m = 80

    val M = randomIntMatrix(n, m)
    val transposeM = M.T

    shape(transposeM) shouldBe ((m, n))

    for {
      row <- 0 until n
      column <- 0 until m
    } transposeM(column)(row) shouldBe M(row)(column)

  }

}
