package org.pfcoperez.dailyalgorithm.database

import org.pfcoperez.dailyalgorithm.datastructures.sets.DisjointSets
import org.scalatest.{FlatSpec, Matchers}

class DisjointSetsSpec extends FlatSpec with Matchers {

  "A DisjointSets datastrucure" should "build consistent unions" in {

    val numbers = 0 to 200

    // Use [[DisjointSets]] to put together numbers ending in the same digit
    val classifiedNumbers = (DisjointSets(numbers:_*) /: numbers) { (dsets, v) =>
      dsets.union(v, v%10)._2
    }

    val groupByClassification = numbers.groupBy(_ % 10).mapValues(_.toSet)
    val (disjointSetsClassification, _) = classifiedNumbers.toSets


    disjointSetsClassification should contain theSameElementsAs groupByClassification

  }

}
