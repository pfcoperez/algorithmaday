package org.pfcoperez.dailyalgorithm

import scala.annotation.tailrec

object Sorting {

  def naiveQuicksort[T](l: List[T])(implicit t2o: T => Ordered[T]): List[T] = l match {
    case pivot :: others =>
      val (lower, higher) = others partition (_ <= pivot)
      List(naiveQuicksort(lower), List(pivot), naiveQuicksort(higher)) flatten
    case _ => Nil
  }

  /*
    Merge sort
    O(n Log n)
   */
  def mergeSort[T](l: List[T])(implicit t2o: T => Ordered[T]): List[T] = {
    @tailrec
    def merge(a: List[T], b: List[T], acc: List[T] = List.empty): List[T] =
      (a, b) match {
        case (ha :: resta, _) if (b.isEmpty || ha <= b.head) => merge(resta, b, ha :: acc)
        case (_, hb :: restb) => merge(a, restb, hb :: acc)
        case _ => acc reverse
      }

    val (a, b) = l.splitAt(l.length / 2)
    merge(if (a.length < 2) a else mergeSort(a), if (b.length < 2) b else mergeSort(b))

  }

}