package org.pfcoperez.dailyalgorithm

import scala.math.Ordering

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

  /**
    * Booth's lexicographic minimal rotation
    * O(n)
    * 
    * Provides the rotation index leading to a minimum lexicographic order
    * & keeping the in-sequence order. 
    */
  def minimalLexicographicRotation[T](s: Vector[T])(implicit ordEvidence: Ordering[T]): Int = {

    implicit class CyclicVector[S](val v: Vector[S]) {
      private def safeIndex(idx: Int): Int =
        (if(idx >= 0) idx else (math.abs(idx)/v.size+1)*v.size + idx) % v.size
      def apply(idx: Int): S = v(safeIndex(idx))
      def update(idx: Int, value: S): CyclicVector[S] = v.updated(safeIndex(idx), value)
    }

    val cs: CyclicVector[T] = s

    import ordEvidence.mkOrderingOps

    def recMinRot(f: CyclicVector[Int], k0: Int, j: Int): Int =
      if(j == s.size*2) k0 else {

        def findIK(i: Int, k: Int): (Int, Int) =
          if(i == -1 || cs(j) == cs(k+i+1)) i -> k
          else findIK(f(i), if(cs(j) < cs(k+i+1)) j-i-1 else k)

        val (i, k) = findIK(f(j-k0-1), k0)

        val (newk, newFjk) =
          if(cs(j) != cs(k+i+1)) (if(cs(j) < cs(k)) j else k, -1)
          else (k, i+1)

        recMinRot(f.update(j-k, newFjk), newk, j+1)
      }

    recMinRot(Vector.fill(s.size)(-1), 0, 1)
  }

}
