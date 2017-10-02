package org.pfcoperez.dailyalgorithm.datastructures.queues

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
    keepInvariants(new CustomQueue[T](heads, x :: tails))

  def dequeue: CustomQueue[T] =
    keepInvariants(
      new CustomQueue(heads.headOption.toList.flatMap(_ => heads.tail), tails))

  /* Average: O(1), Worst: O(n) */
  private def keepInvariants(queue: CustomQueue[T]): CustomQueue[T] =
    (queue.heads, queue.tails) match {
      case (Nil, qtails) =>
        new CustomQueue[T](qtails reverse, Nil)
      case _ => queue
    }

}
