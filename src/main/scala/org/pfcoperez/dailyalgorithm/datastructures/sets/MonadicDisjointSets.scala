package org.pfcoperez.dailyalgorithm.datastructures.sets

import cats._
import cats.data.State

object MonadicDisjointSets {

  def findState[T](v: T) = State[DisjointSets[T], Option[T]](
    disjointSets => disjointSets.find(v))

  def unionState[T](a: T, b: T) = State[DisjointSets[T], Boolean](
    disjointSets => disjointSets.union(a, b))

  def toSets[T] = State[DisjointSets[T], Map[T, Set[T]]](
    disjointSets => disjointSets.toSets)

}
