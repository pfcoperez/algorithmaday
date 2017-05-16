package org.pfcoperez.dailyalgorithm.numericmethods.random

trait RandomGenerator[T] {

  def next: (RandomGenerator[T], T)

}
