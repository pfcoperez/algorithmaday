package org.pfcoperez.dailyalgorithm.datastructures.graphs.directed

import org.scalatest.{ FlatSpec, Matchers }

object DijkstraSpec {

  val E = WeighedNode[Char, Int]('E', Nil)
  val C = WeighedNode[Char, Int]('C', (1, E) :: Nil)
  val D = WeighedNode[Char, Int]('D', (5, C) :: (6, E) :: Nil)
  val B = WeighedNode[Char, Int]('B', (1, C) :: (3, E) :: Nil)
  val A = WeighedNode[Char, Int]('A', (1, B) :: (3, D) :: Nil)

  val root = A

  val expectedDistances = Map(
    'A' -> 0,
    'B' -> 1,
    'C' -> 2,
    'D' -> 3,
    'E' -> 3)

}

class DijkstraSpec extends FlatSpec with Matchers {
  import DijkstraSpec._

  "A dijktra minimun distances algorithm" should "provide the right answer" in {

    val res = minDistancesDijkstra(root) map { case (k, v) => k.value -> v }
    res shouldBe expectedDistances

  }

}
