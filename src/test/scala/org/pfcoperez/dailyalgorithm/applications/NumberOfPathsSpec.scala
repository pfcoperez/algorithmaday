package org.pfcoperez.dailyalgorithm.applications

import org.pfcoperez.dailyalgorithm.datastructures.graphs.directed._
import org.pfcoperez.dailyalgorithm.applications.NumberOfPaths.numberOfPaths
import org.scalatest.{ FlatSpec, Matchers }

class NumberOfPathsSpec extends FlatSpec with Matchers {

  "The `numberOfPaths` method" should "count how many paths, of length 2, are there from nodes 'a' to 'b'" in {

    val D = Node('d', Nil)
    val C = Node('c', D :: Nil)
    val B = Node('b', D :: Nil)
    val A = Node('a', B :: C :: Nil)

    val g: DirectedGraph[Char] = A

    numberOfPaths(g)('a', 'd', 2) shouldBe 2

  }

}
