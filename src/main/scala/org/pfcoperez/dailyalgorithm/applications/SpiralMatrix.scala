package org.pfcoperez.dailyalgorithm.applications

import org.pfcoperez.dailyalgorithm.Algebra.Matrix
import org.pfcoperez.dailyalgorithm.Algebra.Matrix._

object SpiralMatrix extends App {

  /**
   * Produces a sequence with the elements of the given matrix as they are iterated
   * following a clockwise spiral which starts at the position (0, 0)
   *
   * O(n*m), n = number of rows, m = number of columns
   *
   */
  def spiralElements[T](matrix: Matrix[T]): Seq[T] = {

    val (n, m) = size(matrix)

    def spiral(depth: Int, acc: Seq[T]): Seq[T] =

      if (depth >= n / 2 || depth >= m / 2) acc else {

        val left2right = (depth until (m - depth))
        val right2left = left2right.reverse.tail

        val top2bottom = ((depth + 1) until (n - depth))
        val bottom2up = top2bottom.reverse.tail

        val crust = left2right.map(matrix(depth)(_)) ++
          top2bottom.map(matrix(_)(m - 1 - depth)) ++
          right2left.map(matrix(n - 1 - depth)(_)) ++
          bottom2up.map(matrix(_)(depth))

        spiral(depth + 1, crust.reverse ++ acc)

      }

    spiral(0, Seq.empty).reverse

  }

  val n = 4
  val m: Matrix[Int] = positionalValues(n, n)((i, j) => i * n + j)

  println(spiralElements(m))

}
