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

    val result: Option[DisjointSets[T]] = {
      val (opa, phase1) = find(a)
      for {
        pa <- opa
        (opb, phase2) = phase1.find(b)
        pb <- opb

      } yield {
        val ((parent, parentEntry: Entry[T]), (child, childEntry: Entry[T])) = {
          import phase2.{entries => flatEntries}
          val Seq(paEntry, pbEntry) = Seq(pa,pb).map(flatEntries)
          val parent_child = (pa -> paEntry, pb -> pbEntry)
          if(paEntry.rank >= pbEntry.rank) parent_child else parent_child.swap
        }
        new DisjointSets[T] (
          phase2.entries ++ Seq(
            child -> childEntry.copy(parent = parent),
            parent -> parentEntry.copy(rank = Math.max(parentEntry.rank, childEntry.rank+1))
          )
        )
      }
    }
    result.isDefined -> result.getOrElse(this)
  }

  /**
    * Checks whether or not a value is present in the disjoint sets collection
    * @param v label to be found within the data structure
    * @return Check result
    */
  def contains(v: T): Boolean = entries contains v

  /**
    * Find the label of the provided value.
    * @param v Value whose label is to be found
    * @return 'None' if the value doesn't exist, Some(label) otherwise
    */
  def find(v: T): (Option[T], DisjointSets[T]) = {
    val newState = entries.get(v) map { _ =>
      flattenBranch(v)
    }
    (newState map { st => st.entries(v).parent }, newState.getOrElse(this))
  }

  /**
    * Add a value to this datastructure
    * @param v Value to be added
    * @return New [[DisjointSets]]'s state.
    */
  def +(v: T): DisjointSets[T] = {
    if(entries contains v) this
    else new DisjointSets(entries + (v -> Entry(0, v)))
  }

  private def flattenBranch(label: T, toPropagate: List[(T,Entry[T])] = Nil): DisjointSets[T] =
    entries(label) match {
      case Entry(_, parent) if parent == label =>
        val newEntries = entries ++ {
          toPropagate map { case (prevV, prevE) =>
            prevV -> prevE.copy(parent = label)
          }
        }
        new DisjointSets(newEntries)
      case entry @ Entry(_, parent) => flattenBranch(parent, (label, entry)::toPropagate)
    }

}

object DisjointSets {

  def apply[T](labels: T*): DisjointSets[T] = new DisjointSets[T](
    labels.map(l => l -> Entry(0, l)) toMap
  )

  private case class Entry[T](rank: Int, parent: T)
}
