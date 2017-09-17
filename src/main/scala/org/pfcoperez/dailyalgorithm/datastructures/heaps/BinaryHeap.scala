package org.pfcoperez.dailyalgorithm.datastructures.heaps

case class BinaryHeap[T](elements: Vector[T] = Vector.empty)(implicit order: Ordering[T]) {
  import order.mkOrderingOps

  /**
   * Append and element moving it to its priority (order) position.
   * O(log n)
   */
  def enqueue(x: T): BinaryHeap[T] =
    bubbleUp(elements :+ x, elements.size)

  /**
   * Remove the highest priority element or, if two or more share that priority,
   * the latst inserted one.
   *
   * O(log n)
   */
  def dequeue: BinaryHeap[T] =
    bubbleDown(elements.updated(0, elements.last).init, 0)

  def head: T = elements.head

  def isEmpty: Boolean = elements.isEmpty

  def size: Int = elements.size

  private def bubbleUp(queue: Vector[T], idx: Int): BinaryHeap[T] = {
    val parentIdx: Int = idx / 2 + (if (idx % 2 == 0) -1 else 0)
    if (idx == 0 || queue(idx) >= queue(parentIdx)) BinaryHeap(queue)
    else bubbleUp(queue.updated(parentIdx, queue(idx)).updated(idx, queue(parentIdx)), parentIdx)
  }

  private def bubbleDown(queue: Vector[T], idx: Int): BinaryHeap[T] =
    if (idx >= queue.size - 1) BinaryHeap(queue)
    else {
      val swapWith = {
        val candidates = Seq(1, 2).map(_ + idx * 2).filter(_ < queue.size)
        candidates.headOption.map(_ => candidates.minBy(queue))
      }
      if (swapWith.map(swapIdx => queue(idx) < queue(swapIdx)).getOrElse(true))
        BinaryHeap(queue)
      else {
        val swapIdx = swapWith.head
        bubbleDown(queue.updated(swapIdx, queue(idx)).updated(idx, queue(swapIdx)), swapIdx)
      }
    }

}

object BinaryHeap {
  def empty[T: Ordering]: BinaryHeap[T] = BinaryHeap[T]()
}

