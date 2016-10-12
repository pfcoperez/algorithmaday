package org.pfcoperez.dailyalgorithm

import org.pfcoperez.dailyalgorithm.Algebra.Matrix.NumericMatrix.MultiplicationMethod

import scala.reflect.ClassTag

object Algebra {

  object Matrix {

    type Matrix[T] = Array[Array[T]]

    object NumericMatrix {

      abstract class MultiplicationMethod {
        def multiply[T : Numeric : ClassTag](A: Matrix[T], B: Matrix[T]): Matrix[T]
      }

      object Implicits {

        implicit val NaiveMultiplicationMethod = new MultiplicationMethod {

          /**
            * O(n^3)
            */
          override def multiply[T: Numeric : ClassTag](A: Matrix[T], B: Matrix[T]): Matrix[T] = {
            val numericEvidence: Numeric[T] = implicitly[Numeric[T]]
            (0 until A.length) map (i =>
              (0 until B.head.length).map(j =>
                (numericEvidence.zero /: (0 until A.head.length)) {
                  (acc, k) => numericEvidence.plus(acc, numericEvidence.times(A(i)(k), B(k)(j)))
                }
              ) toArray
            ) toArray
          }

        }

      }

    }

    implicit class NumericMatrix[T : Numeric : ClassTag](m: Matrix[T]) {
      def *(that: Matrix[T])(implicit multiplicationMethod: MultiplicationMethod): Matrix[T] =
        multiplicationMethod.multiply(m, that)
    }

    def zeros[T : Numeric : ClassTag](n: Int, m: Int): Matrix[T] =
      Array.fill(n)(Array.fill(m)(implicitly[Numeric[T]].zero))

    def positionalValues[T : Numeric : ClassTag](n: Int, m: Int)(pos2value: (Int, Int) => T): Matrix[T] = {
      val base = zeros[T](n,m)
      // This is confined state mutation within the method body used to improve performance
      for(i <- 0 until n; j <- 0 until m) base(i)(j) = pos2value(i,j)
      base
    }

    def identity[T : Numeric : ClassTag](n: Int, m: Int): Matrix[T] =
      positionalValues(n, m) {
        case (i, j) if i == j => implicitly[Numeric[T]].one
        case _ => implicitly[Numeric[T]].zero
      }

  }

  /* Use example
  import Matrix._
  import Matrix.NumericMatrix.Implicits.NaiveMultiplicationMethod

  val a: Matrix[Int] = Array(Array(1,2,3), Array(4,5,6), Array(7,8,9))
  val b: Matrix[Int] = identity(3,3)

  val res = a*b

  for(row <- res) {
    for(cell <- row) print(s"$cell ")
    println
  }
 */

}
