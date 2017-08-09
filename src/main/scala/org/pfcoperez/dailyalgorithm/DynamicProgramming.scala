package org.pfcoperez.dailyalgorithm

object DynamicProgramming {

  /*
      Optimal (minimum number of used pieces) integer composition from unlimited source
      of pieces of the given types.
      - O(n*m), n = number to be composed, m = number types of pieces to use.
      - Applications: Return change, number of lego pieces to build a tower of a given height, ...
   */
  def minimumIntUnlimitedPieces(pieces: Set[Int])(target: Int): Option[List[Int]] = {
    ((Map(0 -> List.empty[Int])) /: (1 to target)) {

      case (history, subTarget) =>

        val candidatePaths = for (
          candidate <- pieces;
          path2candidate <- history.get(subTarget - candidate)
        ) yield candidate :: path2candidate

        if (candidatePaths isEmpty) history
        else history + (subTarget -> candidatePaths.minBy(_.length))

    }
  } get target

  /**
   * Given a sequence, find all its monotonic sub-sequences
   *
   * O(n^2), n = Number of elements
   *
   * @param s Source sequence.
   * @param ordEv Ordering type class evidence.
   * @tparam T
   * @return All solutions: Sub-sequences of maximum length
   *
   * Example of use:
   *
   * `longestMonotonicSubseqs(Seq(1,2,0,1,-1,4,9,8,7,1,2))`
   *
   */
  def longestMonotonicSubseqs[T](s: Seq[T])(implicit ordEv: Ordering[T]): Seq[Seq[T]] =
    s.headOption map { _ =>
      import ordEv.mkOrderingOps

      val sAsVector = s.toVector
      val l = s.size

      /* Build benefit (sub sequence length) optimization table:
          dpTable(idx) =
            (length of sub-sequence ending at idx, previous sub-sequence element index)
       */
      val dpTable = (Vector(1 -> 0) /: (1 until l)) {
        case (dpTable, i) =>
          val vi = sAsVector(i)
          val prevIdx = (0 until i) maxBy { j =>
            val vj = sAsVector(j)
            if (vj > vi) Int.MinValue else {
              val (jBenefit, _) = dpTable(j)
              jBenefit + 1
            }
          }
          dpTable :+ {
            if (sAsVector(prevIdx) > vi) (1, i)
            else {
              val (prevBenefit, _) = dpTable(prevIdx)
              (prevBenefit + 1, prevIdx)
            }
          }
      }

      // Get the maximum length from the optimization table
      val maxBenefit = dpTable.maxBy(_._1)._1

      /* And use it to start building the solutions, the rabbit holes
         are the last positions of the maximum sub sequences.
      */

      def composeSolution(lastIndex: Int, acc: List[T] = Nil): List[T] = {
        val (_, prevIdx) = dpTable(lastIndex)
        val v = sAsVector(lastIndex)
        if (prevIdx == lastIndex) v :: acc
        else composeSolution(prevIdx, v :: acc)
      }

      (0 until l) collect {
        case idx if dpTable(idx)._1 == maxBenefit =>
          composeSolution(idx)
      }

    } getOrElse Seq.empty

}
