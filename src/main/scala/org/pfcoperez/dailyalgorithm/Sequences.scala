package org.pfcoperez.dailyalgorithm

object Sequences {

  /**
    * Palindromator: Shortest palindrome from a given string
    *
    * O(2^n)
    *
    */
  def shortestPalindromeFrom(s: String): String = {

    def recGen(ss: String, sr: String, acc: (String, String)): String = {
      if(ss.length + sr.length <= 1) s"${acc._1}${ss+sr}${acc._2}"
      else if(ss.isEmpty) recGen(sr.init, sr, acc)
      else if(sr.isEmpty) recGen(ss, ss.init, acc)
      else if(ss.head == sr.head) {
        val v = ss.head
        recGen(ss.tail, sr.tail, (s"${acc._1}$v", s"$v${acc._2}"))
      }
      else Seq(recGen(sr.head + ss, sr, acc), recGen(ss, ss.head + sr, acc)) minBy (_.length)
    }

    val (left, right) = s splitAt (s.length/2)

    recGen(left, right reverse, ("", ""))

  }

  def minimalLexicographicRotation[T](s: Vector[T]): Int = {
    implicit class CyclicVector[S](v: Vector[S]) {
      private def safeIndex(idx: Int): Int = (math.abs(idx).toDouble/v.size).ceil.toInt*v.size + idx
      def apply(idx: Int): S = v(safeIndex(idx))
      def update(idx: Int, value: S): CyclicVector[S] = v.updated(safeIndex(idx), value)
    }
    val cs: CyclicVector[T] = s

    val failure: CyclicVector[Option[T]] = Vector.fill(s.size)(None)

    0

  }

}
