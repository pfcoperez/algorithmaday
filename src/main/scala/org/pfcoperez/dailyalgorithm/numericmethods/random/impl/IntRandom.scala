package org.pfcoperez.dailyalgorithm.numericmethods.random.impl

import org.pfcoperez.dailyalgorithm.numericmethods.random.RandomGenerator

import scala.util.Random

/**
 * Functional interface for an integer random generator
 * which uses Scala's non-FP `scala.util.Random`
 * @param seed
 */
class IntRandom(seed: Long) extends RandomGenerator[Int] {

  private val genImp = new Random(seed)
  override def next: (RandomGenerator[Int], Int) = this -> genImp.nextInt

}
