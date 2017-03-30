package org.pfcoperez.algorithmaday.applications


trait ConnectedComponentsOps {

  def connectedNodes[T](nodes: Set[T], archs: Seq[(T, T)]): Seq[Set[T]] = {
    def connectedNodes(remainingArchs: Seq[(T,T)], components: Union[T]): Seq[Set[T]] =
      if(remainingArchs.isEmpty) components.sets
      else {
        val (from, to) = remainingArchs.head
        connectedNodes(remainingArchs.tail, components.join(from, to))
      }
  
    connectedNodes(archs, emptyUnion)
  }

  protected def emptyUnion[T]: Union[T]

  protected trait Union[T] {
    def join(a: T, b: T): Union[T]
    def sets: Seq[Set[T]]
  }


}

object RegularSetsConnectedComponentsOps extends ConnectedComponentsOps {

}

object DisjointSetsConnectedComponentsOps extends ConnectedComponentsOps {

}
