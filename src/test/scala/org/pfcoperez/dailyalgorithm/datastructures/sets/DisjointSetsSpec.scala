package org.pfcoperez.dailyalgorithm.datastructures.sets

import org.pfcoperez.dailyalgorithm.datastructures.sets.DisjointSets
import org.scalatest.{ FlatSpec, Matchers }

class DisjointSetsSpec extends FlatSpec with Matchers {

  "A DisjointSets datastrucure" should "build consistent unions" in {

    val numbers = 0 to 200

    // Use [[DisjointSets]] to put together numbers ending in the same digit
    val classifiedNumbers = (DisjointSets(numbers: _*) /: numbers) { (dsets, v) =>
      dsets.union(v, v % 10)._1
    }

    val groupByClassification = numbers.groupBy(_ % 10).mapValues(_.toSet)
    val (_, disjointSetsClassification) = classifiedNumbers.toSets

    disjointSetsClassification should contain theSameElementsAs groupByClassification

  }

}
