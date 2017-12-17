package org.pfcoperez.dailyalgorithm.cstheory

import cats.syntax.either._
import cats.instances.either._
import scala.util.{ Either, Right, Left }

import org.pfcoperez.dailyalgorithm.datastructures.graphs.directed

import scala.language.postfixOps

object StateAutomata {

  trait Error
  case class NoTransition[State, Entry](from: State, entry: Entry) extends Error

  case class AutomatonStep[State, Output](
    nextState: State,
    toFinalSt: Boolean,
    output: Option[Output])

  class Automaton[State, Entry, Output] private[StateAutomata] (
    val initialState: State,
    val finalStates: Set[State],
    val transitionMatrix: Map[State, Map[Entry, (State, Option[Output])]]) {

    /**
     * Creates a new stream of transitions: To which state? is it final? what is the transition output?
     * O(1)
     * The generated stream computes each new element in O(1), n transitions are computed in O(n)
     */
    def materializeComputation(
      input: Stream[Entry],
      from: State = initialState): Stream[Either[Error, AutomatonStep[State, Output]]] =
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

    /**
     * Merge two automaton states into a single one
     * O(n + m), n = number of states, m = number of transitions
     */
    def mergeStates(a: State, b: State): Automaton[State, Entry, Output] =
      if (a == b) this
      else {
        val redirectedTransitions = transitionMatrix mapValues {
          _.mapValues {
            case transition @ (to, output) =>
              if (to == b) a -> output
              else transition
          }
        }
        val bTransitions = redirectedTransitions.getOrElse(b, Map.empty)
        val newTransitionMatrix =
          redirectedTransitions + ((a, redirectedTransitions.getOrElse(a, Map.empty) ++ bTransitions))
        val newInitialState = if (initialState == b) a else initialState
        val newFinalStates = finalStates ++ Set(a).filter(_ => finalStates contains b)
        new Automaton(newInitialState, newFinalStates - b, newTransitionMatrix - b)
      }

    lazy val states: Set[State] = transitionMatrix.keySet + initialState

    lazy val unreachableStates: Set[State] = {
      val destinations = {
        for {
          rows <- transitionMatrix.values
          (to, _) <- rows.values
        } yield to
      }.toSet
      states -- destinations
    }

    def removeUnreachable: Automaton[State, Entry, Output] = {
      new Automaton(initialState, finalStates -- unreachableStates, transitionMatrix -- unreachableStates)
    }

  }

  object Automaton {

    /**
     * Produce a minimized version, through Myhill-Nerode theorem, of the input automaton.
     * O(n^2 m^2), n = number of states, m = number of transitions.
     */
    def minimize[State, Entry, Output](automaton: Automaton[State, Entry, Output]): Automaton[State, Entry, Output] = {
      val indexedStates = automaton.states.toVector
      def isFinal(st: State): Boolean = automaton.finalStates contains st

      /* The equivalence classes of states are:
      - States leading to final states after k transitions
      - States leading to non-final states after k transitions */

      val classZero = { // Equivalence classes after 0 transitions
        for {
          i <- 0 until indexedStates.size
          j <- 0 until i
          a = indexedStates(i)
          b = indexedStates(j)
          if (!(isFinal(a) ^ isFinal(b)))
        } yield (a, b)
      } toSet

      /* Equivalence classes after n transitions, up to when no additional
      unions can be performed */
      def classN(prevNonFinalClass: Set[(State, State)]): Set[(State, State)] = {
        val toFinalClass = prevNonFinalClass find {
          case candidate @ (a, b) =>
            val inputsToTest = for {
              st <- Set(a, b)
              inputs <- automaton.transitionMatrix(st).keySet
            } yield inputs
            inputsToTest exists { input =>
              val Seq(stepA, stepB) = Seq(a, b) map { st =>
                for {
                  stTransitions <- automaton.transitionMatrix.get(st)
                  (to, _) <- stTransitions.get(input)
                } yield to
              }
              (stepA zip stepB) exists { candidateTransition =>
                !prevNonFinalClass.contains(candidateTransition)
              }
            }
        }
        if (toFinalClass.nonEmpty) classN(prevNonFinalClass - toFinalClass.get)
        else prevNonFinalClass
      }
      val (newAutomaton, _) = ((automaton, Map.empty[State, State]) /: classN(classZero)) {
        case ((prevAutomaton, equivalences), (pa, pb)) =>
          val Seq(a, b) = Seq(pa, pb).map(s => equivalences.getOrElse(s, s))
          (prevAutomaton.mergeStates(a, b), equivalences + (b -> a))
      }
      newAutomaton.removeUnreachable
    }

  }

  object AutomatonBuilder {

    def startingWith[State, Entry, Output](initial: State): TransitionsBuilder[State, Entry, Output] =
      TransitionsBuilder(initial, Set.empty, Map.empty)

    case class TransitionsBuilder[State, Entry, Output](
      initial: State,
      finalStates: Set[State],
      transitionMatrix: Map[State, Map[Entry, (State, Option[Output])]]) {

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
              from.in, Map.empty[Entry, (State, Option[Output])]) + (when -> (newState, output))
            transitionMatrix + (from.in -> updatedRow)
          }
          val newFinalStates = if (from.isFinal) finalStates + from.in else finalStates
          copy(finalStates = newFinalStates, transitionMatrix = newTransitionMatrix)
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
