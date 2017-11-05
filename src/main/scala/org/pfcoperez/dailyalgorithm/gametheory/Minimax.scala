package org.pfcoperez.dailyalgorithm.gametheory

object Minimax {

  def bestMovement[Movement, State, Score: Ordering](state: State, isMaxPlayer: Boolean, maxDepth: Int)(
    implicit
    transition: (State, Movement) => State,
    validMovements: (State, Boolean) => Seq[Movement],
    score: State => Score): Option[(Movement, Score)] =

    if (maxDepth < 1) None
    else {

      val selectionFunction: Seq[(Movement, Score)] => (Movement, Score) =
        if (isMaxPlayer) _.maxBy(_._2) else _.minBy(_._2)

      val movementCandidates: Seq[(Movement, Score)] =
        validMovements(state, isMaxPlayer) map { movement =>
          val newState = transition(state, movement)
          movement -> {
            bestMovement(newState, !isMaxPlayer, maxDepth - 1) map {
              case (_, subtreeScore) => subtreeScore
            } getOrElse score(newState)
          }
        }

      movementCandidates.headOption map { _ =>
        selectionFunction(movementCandidates)
      }

    }

}
