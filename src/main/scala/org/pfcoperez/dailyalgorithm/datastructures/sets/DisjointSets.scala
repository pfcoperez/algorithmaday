package org.pfcoperez.dailyalgorithm.datastructures.sets

import DisjointSets._

class DisjointSets[T] private(private val entries: Map[T, Entry[T]] = Map.empty) {

  /**
    * Joins two disjoint sets if both are contained by this [[DisjointSets]]
    *
    * @param a Set `a`
    * @param b Set `b`
    * @return (Both labels are contained and joined, new [[DisjointSets]]
    *         with updated state)
    */
  def union(a: T, b: T): (Boolean, DisjointSets[T]) = {

    lazy val result: Option[DisjointSets[T]] = {
      val (opa, phase1) = find(a)
      for {
        pa <- opa
        (opb, phase2) = phase1.find(a)
        pb <- opb
      } yield {
        val ((parent, parentEntry: Entry[T]), (child, childEntry: Entry[T])) = {
          import phase2.{entries => flatEntries}
          val Seq(aEntry, bEntry) = Seq(a,b).map(flatEntries)
          val parent_child = (a -> aEntry, b -> bEntry)
          if(aEntry.rank >= bEntry.rank) parent_child else parent_child.swap
        }
        new DisjointSets[T] (
          phase2.entries ++ Seq(
            child -> childEntry.copy(parent = Some(parent)),
            parent -> parentEntry.copy(rank = Math.max(parentEntry.rank, childEntry.rank+1))
          )
        )
      }
    }
    result.isDefined -> result.getOrElse(this)
  }

  def contains(v: T): Boolean = entries contains v

  def find(v: T): (Option[T], DisjointSets[T]) = {
    val newSt = flattenBranch(v)
    newSt.entries(v).parent -> newSt
  }

  def +(v: T): DisjointSets[T] = {
    if(entries contains v) this
    else new DisjointSets(entries + (v -> Entry(0, None)))
  }

  private def flattenBranch(label: T): DisjointSets[T] =
    entries(label) match {
      case Entry(_, None) => this
      case Entry(rank, Some(parent)) =>
        val Entry(_, grandpa) = entries(parent)
        if(grandpa.isEmpty)
          new DisjointSets[T](entries + (label -> Entry[T](rank, Some(parent))))
        else
          new DisjointSets[T](entries + (label -> Entry(rank, grandpa))) flattenBranch label
    }

}

object DisjointSets {

  def apply[T](labels: T*): DisjointSets[T] = new DisjointSets[T](
    labels.map(l => l -> Entry(0, Option.empty[T])).toMap
  )

  private case class Entry[T](rank: Int, parent: Option[T])
}
