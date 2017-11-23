package org.pfcoperez.dailyalgorithm.cstheory

import org.pfcoperez.dailyalgorithm.cstheory.StateAutomata.{AutomatonBuilder, AutomatonStep, NoTransition}
import org.scalatest.{Inside, Inspectors, Matchers, WordSpec}

class StateAutomataSpec extends WordSpec with Matchers with Inspectors with Inside {

  "StateAutomata" when {

    "modeling finite state machines" should {

      import AutomatonBuilder.startingWith

      "produce finite automata able to recognize regular expressions" in {

        val wordRecognizerAutomaton = startingWith[Int, Char, String](0)
          .in(0).when('g').goTo(1)
          .in(1).when('n').yielding("Almost there!").goTo(2)
          .in(2).when('u').yielding("May the lambda be with you!").goTo(3)
          .inFinal(3).when('!').goTo(0)
          .done

        lazy val infiniteGnus: Stream[Char] = "gnu!".toStream #::: infiniteGnus

        val materializedAutomaton = wordRecognizerAutomaton.materializeComputation(infiniteGnus)

        val results: List[Either[StateAutomata.Error, AutomatonStep[Int, String]]] =
          materializedAutomaton.take(10).toList

        forAll(results)(_.isRight shouldBe true)

        results count {
          case Right(AutomatonStep(_, isFinal, _)) => isFinal
        } shouldBe 2

        forAll(results) {
          case Right(AutomatonStep(toState, true, Some("May the lambda be with you!"))) =>
            toState shouldBe 3
          case _ =>
        }

        val secondMaterializedAutomaton = wordRecognizerAutomaton.materializeComputation("gnua".toStream)

        inside(secondMaterializedAutomaton.last) {
          case Left(NoTransition(fromSt, input)) =>
            fromSt shouldBe 3
            input shouldBe 'a'
        }
      }

    }

  }

}
