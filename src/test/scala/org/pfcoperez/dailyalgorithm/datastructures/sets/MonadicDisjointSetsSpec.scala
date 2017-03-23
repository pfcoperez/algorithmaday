package org.pfcoperez.dailyalgorithm.datastructures.sets

import org.scalatest.{ FlatSpec, Matchers }

import cats._

class MonadicDisjointSetsSpec extends FlatSpec with Matchers {

  "A DisjointSets sets set" should "allow being used using State Monad" in {

    import MonadicDisjointSets._

    val operations = for {
      _ <- unionState(1,2)
      a <- findState(2)
      _ <- unionState(3,4)
      b <- findState(3)
      _ <- unionState(2,3)
      c <- findState(1)
      d <- findState(2)
      e <- findState(3)
      f <- findState(4)
    } yield (a,b,c,d,e,f)

    //val Eval((a, b, c, d, e, f)) = operations.runA(DisjointSets(1,2,3,4))

    //a should not equal (b)


  }

}
