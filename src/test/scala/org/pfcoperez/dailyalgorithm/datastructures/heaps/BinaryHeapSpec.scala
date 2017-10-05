package org.pfcoperez.dailyalgorithm.datastructures.heaps

import org.scalacheck._

class BinaryHeapSpec extends Properties("Heap") {

  /**
   * Sort a sequence elements using a Heap.
   * O(n log n)
   */
  def heapSort[T: Ordering](s: Seq[T]): Seq[T] = {
    val heap = (BinaryHeap.empty[T] /: s)(_.enqueue(_))
    val (l, _) = ((1 to heap.size) :\ (List.empty[T], heap)) {
      case (_, (acc: List[T @unchecked], currentHeap: BinaryHeap[T @unchecked])) =>
        (currentHeap.head :: acc, currentHeap.dequeue)
    }
    l.reverse
  }

  val listGenerator = Gen.listOfN(100, Gen.choose(-1000, 1000))

  property("serve to build heap sort") = Prop.forAll(listGenerator) { seq =>
    heapSort(seq) == seq.sorted
  }

}
