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

  /**
    * Checks whether a sequence if a subsequence of another
    *
    * O(n*m), n = size of s, m = size of ss
    */
  def isSubseq[T](s: Seq[T])(ss: Seq[T]): Boolean =
    (ss /: s) {
      case (Seq(), _) => Seq()
      case (remaining, x) if(remaining.head == x) => remaining.tail
      case _ => ss
    } isEmpty

  /**
    * Search for an element within an cyclic-ordered vector
    * returning its position if found.
    * O(log n)
    */
  def binarySearchInCycle[T](v: Vector[T])(x: T)(
    implicit ordering: Ordering[T], i: Int = 0, j: Int = v.size-1
  ): Option[Int] = {
    import ordering.mkOrderingOps

    val l = j-i+1
    val midIdx = l/2

    if(i > j) None
    else if(v(midIdx) == x) Some(midIdx)
    else {
      type SearchFunction = (Int, Int) => Option[Int]

      val searchFunctions: (SearchFunction, SearchFunction) =
        (binarySearch(v)(x)(ordering, _, _), binarySearchInCycle(v)(x)(ordering, _, _))

      if(v(i) <= v(midIdx)) // Left side ordered
          if(v(i) >= x && x < v(midIdx)) searchFunctions._1(i, midIdx-1)
          else searchFunctions._2(midIdx+1, j)
      else // Right side ordered
          if(v(midIdx)< x && x <= v(j)) searchFunctions._1(midIdx+1, j)
          else searchFunctions._2(i, midIdx-1)

    }

  }

  /**
    * Search for an element within an ordered vector returning its position if found.
    * O(log n)
    */
  def binarySearch[T](v: Vector[T])(x: T)(
    implicit ordering: Ordering[T], i: Int = 0, j: Int = v.size-1
  ): Option[Int] = {
    import ordering.mkOrderingOps

    val l = j-i+1
    val midIdx = i+l/2

    if(i > j) None
    else if(x == v(midIdx)) Some(midIdx)
    else if(x < v(midIdx)) binarySearch(v)(x)(ordering, i, midIdx-1)
    else binarySearch(v)(x)(ordering, midIdx+1, j)
  }

  def rotateLeft[T](v: Vector[T])(idx: Int): Vector[T] = { val (a,b) = v.splitAt(idx); b ++ a }


  /**
    * 
    * Knuth-Morris-Pratt fast sequence matching.
    * O(n), n = Size of the corpus where the target word is to be seached in.
    * 
    */
  def kmpSearch[T](corpus: Seq[T], word: Seq[T]): Seq[Int] = {

    val l = word.size
    val wordVector = word.toVector

    def findOffset(v: T, offset: Int, prefixTable: Vector[Int]): Int =
      if(offset > -1 && v != wordVector(offset+1)) findOffset(v, prefixTable(offset), prefixTable)
      else offset

    val (_, prefixTable) =
      ((-1, Vector.fill(l)(-1)) /: (word.view.zipWithIndex).tail) { case ((k, p) , (v, q)) =>
        val newK = {
          val kv = findOffset(v, k, p)
          kv + { if(wordVector(kv+1) == v) 1 else 0 }
        }
        (newK, p.updated(q, newK))
      }
   
    val (_, results) = ((-1, List.empty[Int]) /:corpus.view.zipWithIndex) {
      case ((q, results), (v, idx)) =>
        val newQ = {
          val qv = findOffset(v, q, prefixTable)
          qv + { if(wordVector(qv+1) == v) 1 else 0 } 
        }
        if(newQ == l-1) (prefixTable(newQ), (idx-l+1)::results)
        else (newQ, results)
    }
    
    results.reverse
  }

}
