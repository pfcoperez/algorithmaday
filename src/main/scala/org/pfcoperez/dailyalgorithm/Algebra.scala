package org.pfcoperez.dailyalgorithm

import scala.reflect.ClassTag

object Algebra {

  type Array[T] = IndexedSeq[T]
  type Matrix[T] = Array[Array[T]]

  object Matrix {

    implicit class PrintableMatrix[T](M: Matrix[T]) {
      override def toString: String =
        (for (row <- M) yield (for (cell <- row) yield cell.toString).mkString(" ")) mkString "\n"
    }

    implicit class TransposableMatrix[T](M: Matrix[T]) {

      /**
       * Transpose matrix: O(1)
       *
       * No need to copy the matrix, just stack the position changes in
       * wrapped delegates.
       *
       */
      def T: Matrix[T] = new Matrix[T] {
        def apply(i: Int): Array[T] = new Array[T] {
          def apply(j: Int): T = M(j)(i)
          def length: Int = M.length
        }

        def length: Int = M.headOption.map(_.length).getOrElse(0)
      }

    }

    object NumericMatrix {

      trait MultiplicationMethod {
        def multiply[T: Numeric: ClassTag](A: Matrix[T], B: Matrix[T]): Matrix[T]
      }

      object MultiplicationMethods {

        object NaiveMultiplicationMethod extends MultiplicationMethod {

          /**
           * O(n^3)
           */
          override def multiply[T: Numeric: ClassTag](A: Matrix[T], B: Matrix[T]): Matrix[T] = {
            val numericEvidence: Numeric[T] = implicitly[Numeric[T]]
            (0 until A.length) map (i =>
              (0 until B.head.length).map(j =>
                (numericEvidence.zero /: (0 until A.head.length)) {
                  (acc, k) => numericEvidence.plus(acc, numericEvidence.times(A(i)(k), B(k)(j)))
                }) toIndexedSeq) toIndexedSeq
          }

        }

        case class DivideAndConquer(fallback: MultiplicationMethod) extends MultiplicationMethod {

          def mergeResults[T: ClassTag](parts: Matrix[Matrix[T]]): Matrix[T] = {
            val (unitN, unitM) = size(parts.head.head)
            positionalValues(unitN * parts.length, unitM * parts.head.length) {
              (i, j) => parts(i / unitN)(j / unitM)(i % unitN)(j % unitM)
            }
          }

          def splitInput[T: ClassTag](X: Matrix[T]): Matrix[Matrix[T]] = {
            val (n, m) = size(X)
            positionalValues(2, 2) {
              (ui, uj) =>
                positionalValues(n / 2, m / 2) {
                  (i, j) => X(ui * n / 2 + i)(uj * n / 2 + j)
                }
            }
          }

          /**
           * O(l^2 Log l), l = n = m
           * PRE: n = m
           */
          override def multiply[T: Numeric: ClassTag](A: Matrix[T], B: Matrix[T]): Matrix[T] = {
            val (n, m) = (size(A)._1, size(B)._2)
            if (n % 2 == 0 && m % 2 == 0) {
              val List(splittedA, splittedB) = List(A, B).map(splitInput)
              mergeResults {
                positionalValues(2, 2) {
                  (ui, uj) =>
                    (zeros(n / 2, m / 2) /: (0 until 2))((sum, k) => sum + multiply(splittedA(ui)(k), splittedB(k)(uj)))
                }
              }
            } else fallback.multiply(A, B)
          }

        }

        case class StrassenDividideAndConquer(fallback: MultiplicationMethod) extends MultiplicationMethod {

          // Let's use regular D&C mutiplication method's auxiliary operations.
          private val divideAndConquerMethod = DivideAndConquer(fallback)
          import divideAndConquerMethod.{ splitInput, mergeResults }

          def multiply[T: Numeric: ClassTag](A: Matrix[T], B: Matrix[T]): Matrix[T] = {
            val (n, m) = (size(A)._1, size(B)._2)
            if (n % 2 == 0 && m % 2 == 0) {

              val aQs = splitInput(A)
              val bQs = splitInput(B)

              val Seq(a11, a12, a21, a22) = aQs.flatten
              val Seq(b11, b12, b21, b22) = bQs.flatten

              val m1 = multiply(a11 + a22, b11 + b22)
              val m2 = multiply(a21 + a22, b11)
              val m3 = multiply(a11, b12 - b22)
              val m4 = multiply(a22, b21 - b11)
              val m5 = multiply(a11 + a12, b22)
              val m6 = multiply(a21 - a11, b11 + b12)
              val m7 = multiply(a12 - a22, b21 + b22)

              mergeResults {
                import scala.Array
                Array(
                  Array(m1 + m4 - m5 + m7, m3 + m5): IndexedSeq[Matrix[T]],
                  Array(m2 + m4, m1 - m2 + m3 + m6): IndexedSeq[Matrix[T]])
              }

            } else fallback.multiply(A, B)
          }

        }

      }

      trait DeterminantMethod {
        def determinant[T: Numeric: ClassTag](M: Matrix[T]): Double
      }

      object DeterminantMethods {

        object LUPDecompositionMethod extends DeterminantMethod {

          /**
           * O(n^3), n = no rows = no columns
           */
          def determinant[T: Numeric: ClassTag](M: Matrix[T]): Double =
            lupDecomposition(M) map {
              case (_, upperMatrix, permutationMatrix, detP) =>
                (1.0 /: (0 until size(M)._1))((prev, k) => prev * upperMatrix(k)(k)) * math.pow(-1.0, detP)
            } getOrElse 0.0

        }

      }

      trait ConvolutionMethod {
        final def convolve[T: Numeric: ClassTag](M: Matrix[T], Kernel: Matrix[T], stride: Int = 1): Matrix[T] = {
          /* Dimensions could be checked at compile time with literal types:
           http://docs.scala-lang.org/sips/pending/42.type.html */
          val (kN, kM) = size(Kernel)
          require(kN == kM && kN % 2 != 0, "Kenel has to have a center and by symmetric")
          convolutionImplementation(M, Kernel, stride)
        }

        protected def convolutionImplementation[T: Numeric: ClassTag](M: Matrix[T], Kernel: Matrix[T], stride: Int): Matrix[T]
      }

      object ConvolutionMethods {

        object DirectComputation extends ConvolutionMethod {

          /**
           * Direct 2D convolution computation
           *
           * O(n*m*f^2),
           * n = no rows, m = no columns, f = kernel number of rows = kernel number of columns.
           *
           */
          def convolutionImplementation[T](M: Matrix[T], Kernel: Matrix[T], stride: Int)(
            implicit
            num: Numeric[T], clsTag: ClassTag[T]): Matrix[T] = {
            import num._
            val (f, _) = size(Kernel)
            val (m, n) = size(M)
            val offset = f / 2
            val Seq(resultN: Int, resultM: Int) = Seq(n, m) map (d => (d - f).toInt / stride + 1)
            positionalValues(resultN, resultM) { (i, j) =>
              val offsets = for {
                ki <- (-offset to offset).view
                kj <- (-offset to offset).view
              } yield (ki, kj)
              val Seq(ci, cj) = Seq(i, j) map { p => offset + p * stride }
              (zero /: offsets) {
                case (acc, (di, dj)) =>
                  acc + M(ci + di)(cj + dj) * Kernel(offset + di)(offset + dj)
              }
            }
          }
        }

      }

      object Implicits {
        implicit val DefaultMultiplicationMethod = MultiplicationMethods.NaiveMultiplicationMethod
        implicit val DefaultDeterminantMethod = DeterminantMethods.LUPDecompositionMethod
        implicit val DefaultConvolutionMethod = ConvolutionMethods.DirectComputation
      }

      /**
       * Lower - Upper - Permutation Decomposition
       * O(n^3)
       *
       * This is a referentially transparent adaptation of Thomas Cormen's algorithm.
       *
       * @param A Square matrix to decompose. Note that the return matrix type isn't T anymore but Double,
       *          that is a constraint imposed by the lack of the division operation in the [[Numeric]] ops interface.
       *
       * @return If the matrix is square and not singular: Some((L, U, P, detP))
       */
      def lupDecomposition[T: Numeric](A: Matrix[T]): Option[(Matrix[Double], Matrix[Double], Matrix[Double], Int)] = {

        val num = implicitly[Numeric[T]]
        import num.mkNumericOps

        def recLUP(A: Matrix[Double], n: Int, K: Int, p: Vector[Int], accDetP: Int): Option[(Matrix[Double], Vector[Int], Int)] = {

          val (prow, pi) = A.zipWithIndex.drop(K) maxBy { case (row, _) => row(K).abs }
          if (prow(K) == 0.0) None //The input can't be a singular matrix
          else {
            val Kprime = pi
            def iK4KPrime(i: Int): Int = if (i == K) Kprime else if (i == Kprime) K else i
            def aik(i: Int): Double = A(iK4KPrime(i))(K) / A(Kprime)(K)

            val newp = p.zipWithIndex map { case (_, K) => p(Kprime); case (_, Kprime) => p(K); case (v, _) => v }

            val newA = positionalValues(n, n) {
              case (i, K) if i > K => aik(i)
              case (i, j) if i > K && j > K => A(iK4KPrime(i))(j) - aik(i) * A(Kprime)(j)
              case (i, j) => A(iK4KPrime(i))(j)
            }
            if (K < n - 1) recLUP(newA, n, K + 1, newp, accDetP + (if (K != Kprime) 1 else 0)) else Some((A, p, accDetP))
          }
        }

        Some(size(A)) collect {
          case (n, m) if n == m && n > 0 =>
            recLUP(fmap(A)(_.toDouble), n, 0, (0 until n) toVector, 0) map {
              case (lu, p, detP) =>
                val L = positionalValues(n, n)((i, j) => if (i == j) 1.0 else if (j < i) lu(i)(j) else 0.0)
                val U = positionalValues(n, n)((i, j) => if (j >= i) lu(i)(j) else 0.0)
                val P = positionalValues(n, n)((i, j) => if (p(i) == j) 1.0 else 0.0)
                (L, U, P, detP)
            }
        } flatten

      }

    }

    implicit class NumericMatrix[T: Numeric: ClassTag](m: Matrix[T]) {

      import org.pfcoperez.dailyalgorithm.Algebra.Matrix.NumericMatrix.{ MultiplicationMethod, DeterminantMethod }
      val numericTypeclass = implicitly[Numeric[T]]
      import numericTypeclass.{ mkNumericOps, zero }

      def *(that: Matrix[T])(implicit multiplicationMethod: MultiplicationMethod): Matrix[T] =
        multiplicationMethod.multiply(m, that)

      def +(that: Matrix[T]): Matrix[T] = {
        val (nrows, ncols) = size(that)
        positionalValues(nrows, ncols)((i, j) => m(i)(j) + that(i)(j))
      }

      def -(that: Matrix[T]): Matrix[T] = {
        val (nrows, ncols) = size(that)
        positionalValues(nrows, ncols)((i, j) => m(i)(j) - that(i)(j))
      }

      def det(implicit determinantMethod: DeterminantMethod): Double =
        determinantMethod.determinant(m)

      def padded(padding: Int): Matrix[T] = new Matrix[T] {

        private val (coreN, coreM) = Matrix.size(m)

        private val zeroRow: Array[T] = new Array[T] {
          def apply(j: Int): T = zero
          def length: Int = coreM + 2 * padding
        }

        def apply(i: Int): Array[T] =
          if ((0 <= i && i < padding) || (coreN <= i && i < coreN + padding)) zeroRow
          else new Array[T] {
            def apply(j: Int): T =
              if ((0 <= j && j < padding) || (coreM <= j && j < coreM + padding)) zero
              else m(i - padding)(j - padding)
            def length: Int = coreN + 2 * padding
          }

        def length: Int = m.headOption.map(_.length).getOrElse(0)
      }

    }

    def zeros[T: Numeric: ClassTag](n: Int, m: Int): Matrix[T] =
      scala.Array.fill(n)(scala.Array.fill(m)(implicitly[Numeric[T]].zero): IndexedSeq[T])

    def positionalValues[T: ClassTag](n: Int, m: Int)(pos2value: (Int, Int) => T): Matrix[T] =
      (0 until n) map (i => (0 until m) map (pos2value(i, _)) toIndexedSeq) toIndexedSeq

    def identity[T: Numeric: ClassTag](n: Int, m: Int): Matrix[T] =
      positionalValues(n, m) {
        case (i, j) if i == j => implicitly[Numeric[T]].one
        case _ => implicitly[Numeric[T]].zero
      }

    def fmap[T, S: ClassTag](M: Matrix[T])(f: T => S): Matrix[S] = {
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
