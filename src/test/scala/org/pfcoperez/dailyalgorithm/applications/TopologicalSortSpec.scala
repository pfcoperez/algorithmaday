package org.pfcoperez.dailyalgorithm.applications

import org.scalatest.{ FlatSpec, Matchers, Inside }

class TopologicalSortSpec extends FlatSpec with Matchers with Inside {
  import TopologicalSort._

  "TopologicalSort algorithm implementation" should "order elements by their dependency tree" in {

    val dm: DependencyMatrix[Char] = Map(
      'A' -> Set('E'),
      'B' -> Set('F'),
      'C' -> Set('G'),
      'D' -> Set('G'),
      'E' -> Set('H'),
      'F' -> Set('H', 'I'),
      'G' -> Set('I'),
      'H' -> Set('J'),
      'I' -> Set('H'),
      'J' -> Set())

    inside(topologicalSort(dm)) {
      case Some(order) =>
        val node2index = order.zipWithIndex.toMap

        node2index('J') shouldBe 0
        node2index('H') shouldBe 1
        node2index('E') should be < node2index('A')
        node2index('F') should be < node2index('B')
        node2index('I') should be < node2index('G')
        node2index('G') should be < node2index('C')
        node2index('G') should be < node2index('D')

    }

  }

  it should "fail to create an order when there are cyclic dependencies" in {

    val dm: DependencyMatrix[Char] = Map(
      'A' -> Set('B'),
      'B' -> Set('C'),
      'C' -> Set('A'))

    topologicalSort(dm) shouldBe empty

  }

  it should "serve to make a plan to develop interstellar travel" in {

    val technologicalDependencies: DependencyMatrix[String] = Map(
      "Interstellar Travel" -> Set("Safe cabins", "Warp drive"),
      "Quantum computing" -> Set(),
      "Magnetic shields" -> Set("Quantum computing"),
      "Dark matter confinement" -> Set("Quantum computing"),
      "Safe cabins" -> Set("Magnetic shields"),
      "Warp drive" -> Set("Dark matter confinement"))

    inside(topologicalSort(technologicalDependencies)) {
      case Some(plan) =>

        plan.head shouldBe "Quantum computing"
        plan.last shouldBe "Interstellar Travel"

        val planIndexes: Map[String, Int] = plan.zipWithIndex.toMap

        planIndexes("Dark matter confinement") should be < planIndexes("Warp drive")
        planIndexes("Magnetic shields") should be < planIndexes("Safe cabins")

    }

  }

}
