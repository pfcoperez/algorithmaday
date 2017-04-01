package org.pfcoperez.algorithmaday.applications


trait ConnectedComponentsOps {

  def connectedNodes[T](nodes: Set[T], archs: Seq[(T, T)]): Seq[Set[T]]

}

object RegularSetsConnectedComponentsOps extends ConnectedComponentsOps {

  def connectedNodes[T](nodes: Set[T], archs: Seq[(T, T)]): Seq[Set[T]] = {
    val g = (nodes.toSeq.map(n => n -> List.empty[T]).toMap /: archs) {
      case (prevG, (from, to)) => prevG + (from -> (to::prevG(from)))
    }

    def dfs(
      toVisit: List[T],
      partition: List[Set[T]],
      visited: Set[T] = Set.empty
    ): Seq[Set[T]] =
      toVisit match {
        case Nil => partition
        case current::remaining =>
          if(visited contains current) dfs(remaining, partition, visited)
          else {
            val children = g(current)
            val updatedPartition: List[Set[T]] = {
              val currentPart::prevParts = partition
              if(currentPart contains current) (currentPart ++ children)::prevParts
              else Set.empty[T]::partition
            }
            dfs(children:::remaining, updatedPartition, visited + current)
          }
      }

    if(nodes.isEmpty) Seq()
    else dfs(nodes.head::Nil, Set(nodes.head)::Nil, Set())

  }

}

object DisjointSetsConnectedComponentsOps extends ConnectedComponentsOps {

  import org.pfcoperez.dailyalgorithm.datastructures.sets.DisjointSets

  def connectedNodes[T](nodes: Set[T], archs: Seq[(T, T)]): Seq[Set[T]] = {
    val dsets = (DisjointSets(nodes.toSeq:_*) /: archs) {
      case (dsets, (from, to)) => dsets.union(from, to)._1
    }
    dsets.toSets._2.values.toSeq
  }

}
