package org.pfcoperez.dailyalgorithm

import scala.annotation.tailrec

object Combinatorics {

  /**
    *
    * Depth first brute force template, a kind of pruned fold where the collection
    * over which it iterates is dynamically generated.
    *
    * O(n^p), n=greatest analysis result size, p=longest chain of states
    *
    * @param z              Starting state.
    * @param analysis       Function which explores what paths starts from the current state.
    * @param isSolution     Function determining whether the current state is a solution or not.
    * @param stepSynthesis  Function which generates a new state from the current and a chosen path.
    * @return               A list of all reached solutions
    *
    */
  def bruteForce[T, S](z: S)(
    analysis: S => List[T])(
    isSolution: S => Boolean)(
    stepSynthesis: (S, T) => S
  ): List[S] = {

    @tailrec
    def bruteForceStep(toExplore: List[S], acc: List[S]): List[S] = toExplore match {
      case st::_ =>
        val newStates = analysis(st) map (stepSynthesis(st, _))
        bruteForceStep(
          newStates:::toExplore.tail,
          newStates.filter(isSolution):::acc)
      case _ => acc
    }

    bruteForceStep(z::Nil, Nil)
  }

}
