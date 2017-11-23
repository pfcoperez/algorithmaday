package org.pfcoperez.dailyalgorithm.cstheory

import cats.syntax.either._
import cats.instances.either._
import scala.util.{ Either, Right, Left }

import org.pfcoperez.dailyalgorithm.datastructures.graphs.directed

object StateAutomata {

  trait Error
  case class NoTransition[State, Entry](from: State, entry: Entry) extends Error

  case class AutomatonStep[State, Output](nextState: State,
                                          toFinalSt: Boolean,
                                          output: Option[Output])

  class Automaton[State, Entry, Output] private[StateAutomata] (
    val initialState: State,
    val finalStates: Set[State],
    private val transitionMatrix: Map[State, Map[Entry, (State, Option[Output])]]) {

    /**
      * Creates a new stream of transitions: To which state? is it final? what is the transition output?
      * O(1)
      * The generated stream computes each new element in O(1), n transitions are computed in O(n)
      */
    def materializeComputation(input: Stream[Entry],
                               from: State = initialState
                              ): Stream[Either[Error, AutomatonStep[State, Output]]] =
      input.scanLeft(AutomatonStep(from, finalStates contains from, Option.empty[Output]).asRight[Error]) {
        case (Right(AutomatonStep(prevSt, _, _)), input) =>
          {
            for {
              stateTransitions <- transitionMatrix.get(prevSt)
              (nextSt, output) <- stateTransitions.get(input)
            } yield AutomatonStep(nextSt, finalStates contains nextSt, output)
          } toRight (NoTransition(prevSt, input))
        case (error, _) => error
      }

    val states: Set[State] = transitionMatrix.keySet + initialState
  }

  object AutomatonBuilder {

    def startingWith[State, Entry, Output](initial: State): TransitionsBuilder[State, Entry, Output] =
      TransitionsBuilder(initial, Set.empty, Map.empty)

    case class TransitionsBuilder[State, Entry, Output](
      initial: State,
      finalStates: Set[State],
      transitionMatrix: Map[State, Map[Entry, (State, Option[Output])]]
    ) {

      def in(state: State) = FromSt(state)
      def inFinal(state: State) = FromSt(state, true)

      def done: Automaton[State, Entry, Output] =
        new Automaton(initial, finalStates, transitionMatrix)

      case class FromSt(in: State, isFinal: Boolean = false) {
        def when(entry: Entry) = Transition(this, entry)
      }

      trait ToSt {

        val from: FromSt
        val when: Entry
        val output: Option[Output]

        def goTo(newState: State): TransitionsBuilder[State, Entry, Output] = {
          val newTransitionMatrix = {
            val updatedRow = transitionMatrix.getOrElse(
              from.in, Map.empty[Entry, (State, Option[Output])]
            ) + (when -> (newState, output))
            transitionMatrix + (from.in -> updatedRow)
          }
          val newFinalStates = if(from.isFinal) finalStates + from.in else finalStates
          copy(finalStates = newFinalStates,transitionMatrix = newTransitionMatrix)
        }

      }

      case class Transition(from: FromSt, when: Entry) extends ToSt {
        override val output: Option[Output] = None

        def yielding(output: Output) = Yielding(this, Some(output))
      }

      case class Yielding(to: ToSt, output: Option[Output]) extends ToSt {
        override val from: FromSt = to.from
        override val when: Entry = to.when
      }

    }

  }

}
