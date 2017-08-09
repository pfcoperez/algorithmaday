package org.pfcoperez.dailyalgorithm

object Databases {

  implicit class JoinOperations[K, V](thisMap: Map[K, V]) {

    /**
     * Inner join like operation
     * O(n), n = left map size
     */
    def joinInner[VT](that: Map[K, VT]): Map[K, (V, VT)] =
      for ((k, va) <- thisMap; vb <- that.get(k)) yield k -> (va, vb)

    /**
     * Outer left join like operation
     * O(n), n = left map size
     */
    def joinOuterLeft[VT](that: Map[K, VT]): Map[K, (V, Option[VT])] =
      for ((k, va) <- thisMap) yield k -> (va, that.get(k))

    /**
     * Right outer join like operation
     * O(n), n = right map size
     */
    def joinOuterRight[VT](that: Map[K, VT]): Map[K, (Option[V], VT)] =
      for ((k, vb) <- that) yield k -> (thisMap.get(k), vb)

  }

}
