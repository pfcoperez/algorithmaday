package org.pfcoperez.dailyalgorithm

import org.pfcoperez.dailyalgorithm.Statistics.OnLineMedian.PriorityQueue

object Statistics {

  /*
     Bayer-Moore majority vote search with verification.
     O(n)
   */
  def majority[T](s: Seq[T]): Option[T] = {
    (Option.empty[(T, Long)] /: s) {
      case (None, v) => Some(v -> 1)
      case (Some((vc, c)), v) =>
        Some((vc, c + (if (v == vc) 1 else -1))) filter (_._2 > 0)
    } collect {
      case (candidateValue, _) if s.count(_ == candidateValue) > s.length / 2 =>
        candidateValue
    }
  }

  /*
     Median calculation using a quick sort like algorithm which reduces execution time & memory usage
     (compared to classic approach by which the list gets ordered and it central element chosen) for
     those cases in which the median is close to the center of the list.
     O(n Log n)
   */
  def quickMedian[T](l: List[T])(implicit t2o: T => Ordered[T]): Option[T] = {

    def quickMedianRec(l: List[T], emp: Int): Option[T] = l.headOption flatMap { pivot =>
      val (under, over) = l.tail partition (_ <= pivot)
      if (under.length == emp) Some(pivot)
      else if (under.length > emp) quickMedianRec(under, emp)
      else quickMedianRec(over, emp - under.length - 1)
    }

    quickMedianRec(l, l.length / 2)

  }

  /*
    Online immutable median calculator
   */
  object OnLineMedian {

    trait PriorityQueue[T] {
      def enqueue(t: T): PriorityQueue[T]
      def popHead: PriorityQueue[T]
      def headOption: Option[T]
      def head: T = headOption.head
      def size: Int
      def isEmpty: Boolean = size == 0
    }

    object PriorityQueue {

      def moveHead[T](from: PriorityQueue[T], to: PriorityQueue[T]): (PriorityQueue[T], PriorityQueue[T]) = {
        val v: T = from.head //If from was empty, an unrecoverable error will raise
        from.popHead -> to.enqueue(v)
      }

    }

    def apply[T](queueFactory: Ordering[T] => PriorityQueue[T])(implicit cmp: Ordering[T]) =
      new OnLineMedian(queueFactory(cmp), queueFactory(cmp.reverse))

  }

  class OnLineMedian[T: Ordering] private (private val lower: PriorityQueue[T], private val upper: PriorityQueue[T]) {

    import PriorityQueue.moveHead

    private val cmp = implicitly[Ordering[T]]

    /**
     * O(Log n)
     */
    def nextSample(s: T): OnLineMedian[T] = {

      val newQueues =
        if (lower.isEmpty || cmp.gt(s, lower.head)) (lower, upper enqueue s)
        else if (upper.isEmpty || cmp.lt(s, upper.head)) (lower enqueue s, upper)
        else if (lower.size > upper.size) (lower enqueue s, upper)
        else (lower, upper enqueue s)

      (new OnLineMedian[T](_, _)) tupled balanceQueues.tupled(newQueues)

    }

    /**
     * O(1)
     */
    def currentMedian: Set[T] = lower.headOption.toSet ++ upper.headOption.toSet

    /**
     * O(Log n)
     */
    private[this] val balanceQueues: (PriorityQueue[T], PriorityQueue[T]) => (PriorityQueue[T], PriorityQueue[T]) = {
      case (l, u) if (l.size - u.size == -2) => moveHead(u, l) swap
      case (l, u) if (l.size - u.size == 2) => moveHead(l, u)
      case balanced => balanced
    }

  }

  /**
   * Closest elements in a collection given a distance for their type
   * O(n Log n)
   * @param elements
   * @param tMin Reference frame for elements ordering determine by the provided distance
   * @param distance Function providing the distance between two elements of type T
   *
   * Use example: closestElements(Seq(1,10,11,23))(0) { case (a,b) => a-b }
   */
  def closestElements[T](elements: Seq[T])(tMin: T)(distance: (T, T) => Int): List[(T, T)] = {

    implicit val cmp = new Ordering[T] {
      override def compare(x: T, y: T): Int = distance(tMin, x) - distance(tMin, y)
    }

    val sorted = elements.sorted
    val diffs = sorted.view zip sorted.view.tail

    val minDiffPair = diffs.headOption.map(_ => diffs minBy distance.tupled).toList

    minDiffPair flatMap {
      case (a, b) => diffs.filter { case (c, d) => distance(c, d) <= distance(a, b) }
    }

  }

  /**
   * Kadane's algorithm to get the maximum-sum sub sequence of a sequence of ordered elements
   * (with zero and "negative elements")
   *
   * O(n), n = size of the input sequence
   *
   */
  def maximumSumSubSequence[T](s: Seq[T])(implicit num: Numeric[T]): Seq[T] = {

    val (maxSum, sums) = ((num.zero, List.empty[(T, T)]) /: s) {
      case ((maxSum, prevSums), value) =>
        val maxSumUpToValue = num.max(value, num.plus(value, prevSums.headOption.map(_._1) getOrElse num.zero))
        (num.max(maxSumUpToValue, maxSum), (maxSumUpToValue -> value) :: prevSums)
    }

    sums.view dropWhile (_._1 != maxSum) takeWhile {
      case (sum, _) => num.gt(sum, num.zero)
    } reverseMap (_._2)

  }

}

