package org.pfcoperez.dailyalgorithm

import org.scalatest.{FlatSpec, Matchers}
import org.scalatest.concurrent.TimeLimits
import org.scalatest.time._

class NumbersSpec extends FlatSpec with Matchers with TimeLimits {
  import Numbers.compiledPrimes

  "Compile time prime numbers generation" should "take O(n) run-time" in {

    val initTimestamp = System.currentTimeMillis()
    val first100primes = compiledPrimes(10)
    val referenceTime = math.max(System.currentTimeMillis()-initTimestamp, 1L)*2L

    val first1000primes = failAfter(Span(referenceTime*10, Milliseconds)) {
      compiledPrimes(100)
    }

    val first10000primes = failAfter(Span(referenceTime*100, Milliseconds)) {
      compiledPrimes(1000)
    }

    val primesSets = List(first100primes, first1000primes, first10000primes).map(_.toSet)

    (primesSets zip primesSets.tail) foreach { case (smaller, bigger) =>
      bigger should contain allElementsOf smaller
    }


  }

}
