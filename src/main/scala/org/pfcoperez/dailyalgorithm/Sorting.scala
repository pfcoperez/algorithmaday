package org.pfcoperez.dailyalgorithm

object Sorting {

  def naiveQuicksort[T](l: List[T])(implicit t2o: T => Ordered[T]): List[T] = l match {
    case pivot::others =>
      val (lower, higher) = others partition (_ <= pivot)
      List(naiveQuicksort(lower), List(pivot), naiveQuicksort(higher)) flatten
    case _ => Nil
  }

}