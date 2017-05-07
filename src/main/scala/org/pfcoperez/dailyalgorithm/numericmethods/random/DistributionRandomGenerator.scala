package org.pfcoperez.dailyalgorithm.numericmethods.random

import org.pfcoperez.dailyalgorithm.numericmethods.random.DistributionRandomGenerator.DensityFunction
import org.pfcoperez.dailyalgorithm.numericmethods.random.impl.DoubleRandomGen

import scala.annotation.tailrec


class DistributionRandomGenerator private (
                                            private val randomDoubleGen: RandomGenerator[Double],
                                            val densityFunction: DensityFunction
                                          ) extends RandomGenerator[Double] {

  final override def next: (DistributionRandomGenerator, Double) = {

    def normalize(x: Double): Double =
      if(x < 0.0) normalize(-x)
      else if(x > 1.0) normalize(x - x.floor)
      else x

    def withinRange(x: Double, range: (Double, Double)): Double = {
      val (a, b) = range
      a + normalize(x)*(b-a)
    }

    val (nextDoubleGenA, rawX) = randomDoubleGen.next
    val (nextDoubleGenB, rawY) = nextDoubleGenA.next

    val updatedGenerator = new DistributionRandomGenerator(nextDoubleGenB, densityFunction)

    val Seq(x, y) = Seq(rawX, rawY) map (withinRange(_, densityFunction.domain))

    val candidate = densityFunction.lift(x) collect {
      case maxY if y >= 0.0 && y <= maxY => updatedGenerator -> x
    }

    if(candidate.isDefined) candidate.get
    else updatedGenerator.next

  }


}

object DistributionRandomGenerator {

  import scala.runtime.AbstractPartialFunction

  case class DensityFunction(domain: (Double, Double))(
    f: Double => Double
  ) extends AbstractPartialFunction[Double, Double] {

    override def applyOrElse[A1 <: Double, B1 >: Double](x: A1, default: (A1) => B1): B1 = {
        if (isDefinedAt(x)) f(x) else default(x)
    }

    override def isDefinedAt(x: Double): Boolean = {
      val (from, to) = domain
      from <= x && x <= to
    }

    def validateDensity(step: Double, tolerance: Option[Double] = None): Boolean = {
      val (a, b) = domain
      val acceptedError = tolerance.getOrElse(2 * step)
      val p = (a to b by step).map(f).sum * step
      math.abs(p - 1.0) <= acceptedError
    }

  }

  object DensityFunctions {

    def uniform(from: Double, to: Double): DensityFunction =
      DensityFunction(from, to)(_ => 1.0/(from - to))



  }

  def apply(df: DensityFunction, seed: Long): DistributionRandomGenerator = {
    new DistributionRandomGenerator(new DoubleRandomGen(seed), df)
  }
}
