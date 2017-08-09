package org.pfcoperez.dailyalgorithm.numericmethods.random.impl

import org.pfcoperez.dailyalgorithm.numericmethods.random.RandomGenerator

import scala.util.Random

/**
 * Functional interface for a Double random generator
 * which uses Scala's non-FP `scala.util.Random`
 * @param seed
 */
class DoubleRandomGen(seed: Long) extends RandomGenerator[Double] {

  private val genImp = new Random(seed)
  override def next: (RandomGenerator[Double], Double) = this -> genImp.nextDouble

}
