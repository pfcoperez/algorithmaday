package org.pfcoperez.dailyalgorithm.algebra

import org.pfcoperez.dailyalgorithm.Algebra.Matrix._
import org.pfcoperez.dailyalgorithm.Algebra.Matrix.NumericMatrix._
import org.scalatest.{ FlatSpec, Matchers }

class LUPDecompositionSpec extends FlatSpec with Matchers {

  "The Cormen's L-U-P Decomposition algorithm implementation" should "be able to decompose a 4x4 int matrix" in {

    val M: Matrix[Double] = Array(
      Array(2, 0, 2, 0.6),
      Array(3, 3, 4, -2),
      Array(5, 5, 4, 2),
      Array(-1, -2, 3.4, -1))

    val Some((l, u, p, _)) = lupDecomposition(M)

    import NumericMatrix.Implicits.DefaultMultiplicationMethod

    val PM = p * M
    val LU = l * u

    PM.deep should be(LU)

  }

}
