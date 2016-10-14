package org.pfcoperez.dailyalgorithm


import scala.reflect.ClassTag

object Algebra {

  object Matrix {

    type Matrix[T] = Array[Array[T]]

    object NumericMatrix {

      trait MultiplicationMethod {
        def multiply[T: Numeric : ClassTag](A: Matrix[T], B: Matrix[T]): Matrix[T]
      }

      object MultiplicationMethods {

        object NaiveMultiplicationMethod extends MultiplicationMethod {

          /**
            * O(n^3)
            **/
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

        case class DivideAndConquer(fallback: MultiplicationMethod) extends MultiplicationMethod {

          protected def mergeResults[T: ClassTag](parts: Matrix[Matrix[T]]): Matrix[T] = {
            val (unitN, unitM) = size(parts.head.head)
            positionalValues(unitN * parts.length, unitM * parts.head.length) {
              (i, j) => parts(i / unitN)(j / unitM)(i % unitN)(j % unitM)
            }
          }

          protected def splitInput[T: ClassTag](X: Matrix[T]): Matrix[Matrix[T]] = {
            val (n, m) = size(X)
            positionalValues(2, 2) {
              (ui, uj) => positionalValues(n / 2, m / 2) {
                (i, j) => X(ui * n / 2 + i)(uj * n / 2 + j)
              }
            }
          }

          /**
            *
            */
          override def multiply[T: Numeric : ClassTag](A: Matrix[T], B: Matrix[T]): Matrix[T] = {
            val (n, m) = (size(A)._1, size(B)._2)
            if (n % 2 == 0 && m % 2 == 0) {
              val List(splittedA, splittedB) = List(A,B).map(splitInput)
              mergeResults {
                positionalValues(2, 2) {
                  (ui, uj) =>
                    (zeros(n / 2, m / 2) /: (0 until 2))((sum,k) => sum + multiply(splittedA(ui)(k),splittedB(k)(uj)))
                }
              }
            } else fallback.multiply(A, B)
          }

        }

      }

      object Implicits {
        implicit val DefaultMultiplicationMethod = MultiplicationMethods.NaiveMultiplicationMethod
      }


    }

    implicit class NumericMatrix[T: Numeric : ClassTag](m: Matrix[T]) {

      import org.pfcoperez.dailyalgorithm.Algebra.Matrix.NumericMatrix.MultiplicationMethod

      def *(that: Matrix[T])(implicit multiplicationMethod: MultiplicationMethod): Matrix[T] =
        multiplicationMethod.multiply(m, that)

      def +(that: Matrix[T]): Matrix[T] = {
        val (nrows, ncols) = size(that)
        positionalValues(nrows, ncols)((i, j) => implicitly[Numeric[T]].plus(m(i)(j), that(i)(j)))
      }
    }

    def zeros[T: Numeric : ClassTag](n: Int, m: Int): Matrix[T] =
      Array.fill(n)(Array.fill(m)(implicitly[Numeric[T]].zero))

    def positionalValues[T: ClassTag](n: Int, m: Int)(pos2value: (Int, Int) => T): Matrix[T] =
      (0 until n) map (i => (0 until m) map (pos2value(i, _)) toArray) toArray

    def identity[T: Numeric : ClassTag](n: Int, m: Int): Matrix[T] =
      positionalValues(n, m) {
        case (i, j) if i == j => implicitly[Numeric[T]].one
        case _ => implicitly[Numeric[T]].zero
      }

    def fmap[T, S: ClassTag](M: Matrix[T], f: T => S): Matrix[S] = {
      val (n, m) = size(M)
      positionalValues(n, m)((i, j) => f(M(i)(j)))
    }

    def size[T](m: Matrix[T]): (Int, Int) = m.length -> m.head.length

  }


  /* Use example
  import Matrix._
  //import Matrix.NumericMatrix.Implicits.DefaultMultiplicationMethod
  import org.pfcoperez.dailyalgorithm.Algebra.Matrix.NumericMatrix.MultiplicationMethod
  import org.pfcoperez.dailyalgorithm.Algebra.Matrix.NumericMatrix.MultiplicationMethods.DivideAndConquer

  implicit val _: MultiplicationMethod = DivideAndConquer(Matrix.NumericMatrix.MultiplicationMethods.NaiveMultiplicationMethod)

  val a: Matrix[Int] = Array(Array(1, 2, 3, 4), Array(5, 6, 7, 8), Array(9, 10, 11, 12), Array(13, 14, 15, 16))
  val b: Matrix[Int] = identity(4, 4)

  val res = a * b

  printMatrix(res)

  def printMatrix[T](m: Matrix[T]): Unit = {
    for (row <- m) {
      for (cell <- row) print(s"$cell ")
      println
    }
  }*/


}
