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

}
