package org.pfcoperez.dailyalgorithm.datastructures.heaps

import scala.language.higherKinds

trait IndexedSeqClass[F[_]] {

  def size(f: F[_]): Int
  def get[T](f: F[T])(idx: Int): T
  def update[T](f: F[T])(idx: Int, v: T): F[_]
  def append[T](f: F[T])(v: T): F[T]
  def prepend[T](f: F[T])(v: T): F[T]

}

trait Heap[F[_]] {

  /**
   * O(log n)
   * As long as F's indexed access is O(1)
   */
  final def enqueue[T: Ordering](elements: F[T])(x: T)(implicit isc: IndexedSeqClass[F]): F[T] = {
    import isc._
    bubbleUp(append(elements)(x), size(elements))
  }

  /**
   * O(log n)
   * As long as F's indexed access is O(1)
   */
  final def dequeue[T: Ordering](elements: F[T])(implicit isc: IndexedSeqClass[F]): F[T] = {
    import isc._
    bubbleDown(update(elements)(0, elements.last).init, 0)
  }

  /**
   * O(log n)
   * As long as F's indexed access is O(1)
   */
  protected def bubbleUp[T](elements: F[T], idx: Int)(implicit ordering: Ordering[T]): F[T] = {
    import ordering.mkOrderingOps

    val parentIdx: Int = idx / 2 + (if (idx % 2 == 0) -1 else 0)

    if (idx == 0 || elements(idx) >= elements(parentIdx))
      elements
    else
      bubbleUp(elements.updated(parentIdx, elements(idx)).updated(idx, elements(parentIdx)), parentIdx)
  }

  /**
   * O(log n)
   * As long as F's indexed access is O(1)
   */
  protected def bubbleDown[T](elements: F[T], idx: Int)(implicit ordering: Ordering[T]): F[T] =
    if (idx >= elements.size - 1) elements
    else {
      import ordering.mkOrderingOps

      val leftIdx = idx * 2 + 1
      val rightIdx = idx * 2 + 2

      val swapWith = Seq(leftIdx, rightIdx).filter(_ < elements.size).minBy(elements)

      if (elements(idx) < elements(swapWith)) elements
      else bubbleDown(elements.updated(swapWith, elements(idx)).updated(idx, queue(swapWith)), swapWith)

    }

}

object Heap {

  object instances {
    implicit object VectorHeap extends Heap[Vector]
  }

  object syntax {
    implicit class vectorSyntax[T: Ordering](v: Vector[T]) {
      def enqueue(x: T): Vector[T] = enqueue(v)(x)
      def queueTail: Vector[T] = dequeue(x)
    }
  }

}

