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

}

