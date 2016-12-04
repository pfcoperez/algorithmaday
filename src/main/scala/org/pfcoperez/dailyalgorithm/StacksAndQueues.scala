package org.pfcoperez.dailyalgorithm

object StacksAndQueues {

  object CustomQueue {
    def empty[T]: CustomQueue[T] = new CustomQueue[T](Nil, Nil)
  }

  class CustomQueue[T] private (
                                 private val heads: List[T],
                                 private val tails: List[T]) {

    /* O(1) */
    def head: Option[T] = heads.headOption

    /* Average: O(1) */
    def enqueue(x: T): CustomQueue[T] =
      keepInvariants(new CustomQueue[T](heads, x::tails))

    def dequeue: CustomQueue[T] =
      keepInvariants(
        new CustomQueue(heads.headOption.toList.flatMap(_ => heads.tail), tails)
      )

    /* Average: O(1), Worst: O(n) */
    private def keepInvariants(queue: CustomQueue[T]): CustomQueue[T] =
      (queue.heads, queue.tails) match {
        case (Nil, qtails) =>
          new CustomQueue[T](qtails reverse, Nil)
        case _ => queue
      }

  }

  object Heap {

    type Heap[T] = Vector[T]

    def enqueue[T : Ordering](heap: Heap[T])(x: T): Heap[T] =
      bubbleUp(heap :+ x, heap.size)

    def dequeue[T : Ordering](heap: Heap[T]): Heap[T] =
      bubbleDown(heap.updated(0, heap.last).init, 0)

    private def bubbleUp[T](queue: Heap[T], idx: Int)(implicit ordering: Ordering[T]): Heap[T] = {
      import ordering.mkOrderingOps

      val parentIdx: Int = idx/2 + (if(idx%2 == 0) -1 else 0)

      if(idx == 0 || queue(idx)>=queue(parentIdx))
        queue
      else
        bubbleUp(queue.updated(parentIdx, queue(idx)).updated(idx, queue(parentIdx)), parentIdx)
    }

    private def bubbleDown[T](queue: Heap[T], idx: Int)(implicit ordering: Ordering[T]): Heap[T] =
      if(idx >= queue.size-1) queue
      else {
        import ordering.mkOrderingOps

        val leftIdx = idx*2+1
        val rightIdx = idx*2+2

        val swapWith = Seq(leftIdx, rightIdx).filter(_ < queue.size).minBy(queue)

        if(queue(idx) < queue(swapWith)) queue
        else bubbleDown(queue.updated(swapWith, queue(idx)).updated(idx, queue(swapWith)), swapWith)

      }

  }

}
