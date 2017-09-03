package org.pfcoperez.dailyalgorithm.algebra

import org.pfcoperez.dailyalgorithm.Algebra.Matrix
import org.pfcoperez.dailyalgorithm.Algebra.Matrix._
import org.pfcoperez.dailyalgorithm.Algebra.Matrix.NumericMatrix._
import org.scalatest.{ FlatSpec, Matchers }

class LUPDecompositionSpec extends FlatSpec with Matchers {

  "The Cormen's L-U-P Decomposition algorithm implementation" should "be able to decompose a 4x4 int matrix" in {

    val M: Matrix[Double] = IndexedSeq(
      IndexedSeq(2, 0, 2, 0.6),
      IndexedSeq(3, 3, 4, -2),
      IndexedSeq(5, 5, 4, 2),
      IndexedSeq(-1, -2, 3.4, -1))

    val Some((l, u, p, _)) = lupDecomposition(M)

    import NumericMatrix.Implicits.DefaultMultiplicationMethod

    val PM = p * M
    val LU = l * u

    PM.map(_.toArray).toArray.deep should be(LU)

  }

}
