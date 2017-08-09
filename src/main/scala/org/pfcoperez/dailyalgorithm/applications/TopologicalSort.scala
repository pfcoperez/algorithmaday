package org.pfcoperez.dailyalgorithm.applications

object TopologicalSort {

  type DependencyMatrix[T] = Map[T, Set[T]]

  /**
   * TopologicalSort: Kahn's algorithm
   * O(n), n = number of archs in the input DAG
   *
   * @param dm DAG as a dependency matrix.
   *
   */
  def topologicalSort[T](dm: DependencyMatrix[T]): Option[List[T]] = {

    val destinations0 = dm.values.flatten.groupBy(identity).mapValues(_.size)
    val s0 = dm.keySet.filterNot(destinations0 contains _)

    def topologicalSort(
      s: Set[T],
      destinations: Map[T, Int],
      dm: DependencyMatrix[T])(acc: List[T]): Option[List[T]] = {
      val (sh, remaining) = s.splitAt(1)
      if (sh.isEmpty) Some(acc) filter { _ =>
        destinations.values.forall(_ == 0)
      }
      else {
        val from = sh.head
        val targets = dm.getOrElse(from, Set.empty)
        val (newS, newDestinations, newDm) =
          ((remaining, destinations, dm) /: targets) {
            case ((s, destinations, dm), to) =>
              val archs2To = destinations(to)
              (
                if (archs2To == 1) s + to else s,
                destinations + (to -> (archs2To - 1)),
                dm + (from -> (targets - to)))
          }
        topologicalSort(newS, newDestinations, newDm)(from :: acc)
      }
    }

    topologicalSort(s0, destinations0, dm)(Nil)
  }

}

