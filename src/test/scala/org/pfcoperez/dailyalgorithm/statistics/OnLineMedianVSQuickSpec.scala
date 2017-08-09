package org.pfcoperez.dailyalgorithm.statistics

import org.pfcoperez.dailyalgorithm.Statistics.OnLineMedian.PriorityQueue
import org.pfcoperez.dailyalgorithm.Statistics._
import org.scalatest.{ FlatSpec, Matchers }
import org.scalacheck.Gen._

class OnLineMedianVSQuickSpec extends FlatSpec with Matchers {

  (50 to 1000) foreach { sampleSize =>

    val genInput = listOfN(sampleSize, choose(-1000, 1000))
    val sample = genInput.sample.get

    "OnLineMedian algorithm" should s"produce the same results than QuickMedian: $sample" in {

      val emptyMedian = OnLineMedian[Int] { ord =>
        new PriorityQueue[Int] {
          // This is an immutable interface implementation using Scala's mutable PriorityQueue
          val underlying = collection.mutable.PriorityQueue.empty[Int](ord)

          def enqueue(t: Int): PriorityQueue[Int] = {
            underlying.enqueue(t); this
          }

          def popHead: PriorityQueue[Int] = {
            underlying.dequeue(); this
          }

          def headOption: Option[Int] = underlying.headOption

          def size: Int = underlying.size
        }
      }

      val om = (emptyMedian /: sample) {
        case (currentMedian, currentVal) => currentMedian.nextSample(currentVal)
      }

      val qm = quickMedian(sample).get
      val onlineMedian: Set[Int] = om.currentMedian

      onlineMedian should contain(qm)

    }

  }

}