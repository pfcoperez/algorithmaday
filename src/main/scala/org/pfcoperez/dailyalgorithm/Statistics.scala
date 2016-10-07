package org.pfcoperez.dailyalgorithm

object Statistics {

  /*
     Bayer-Moore majority vote search with verification.
     O(n)
   */
  def majority[T](s: Seq[T]): Option[T] = {
    (Option.empty[(T, Long)] /: s) {
      case (None, v) => Some(v -> 1)
      case (Some((vc, c)), v) =>
        Some((vc, c + (if(v == vc) 1 else -1))) filter (_._2 > 0)
    } collect { case (candidateValue, _)
        if s.count(_ == candidateValue) > s.length/2 =>
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
      if(under.length == emp) Some(pivot)
      else if(under.length > emp) quickMedianRec(under, emp)
      else quickMedianRec(over, emp-under.length-1)
    }

    quickMedianRec(l, l.length/2)

  }

}

