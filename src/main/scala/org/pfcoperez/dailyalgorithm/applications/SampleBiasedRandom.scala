package org.pfcoperez.dailyalgorithm.applications

import org.pfcoperez.dailyalgorithm.numericmethods.random.DistributionRandomGenerator
import org.pfcoperez.dailyalgorithm.numericmethods.random.DistributionRandomGenerator.DensityFunction

object SampleBiasedRandom extends App {

  val sampleSize = 1000
  val range = (-1.0, 3.0)
  val intervalWidth = 0.1

  def genSample(size: Int, seed: Long, distribution: DensityFunction): List[Double] = {

    def genSample(size: Int)(acc: List[Double] = Nil, generator: DistributionRandomGenerator): List[Double] =
      if (size == 0) acc else {
        val (nextGen, nextValue) = generator.next
        genSample(size - 1)(nextValue :: acc, nextGen)
      }

    genSample(size)(Nil, DistributionRandomGenerator(distribution, seed))
  }

  val sample =
    genSample(1000, 12122, DistributionRandomGenerator.DensityFunctions.normal(1.0, 0.25)(range))

  def intervalFrequencies(
    sample: Seq[Double])(
    range: (Double, Double),
    intervalWidth: Double): Seq[((Double, Double), Int)] = {

    val classifiedSample: Map[Int, Seq[Double]] = sample.groupBy(x => (x / intervalWidth).toInt)

    classifiedSample.toSeq.sortBy(_._1) map {
      case (intNo, samples) =>
        (intNo * intervalWidth, (intNo + 1) * intervalWidth) -> samples.size
    }

  }

  println {
    s"""
       |
       |
       |Stats:
       |------
     """.stripMargin + "\n" + {
      intervalFrequencies(sample)(range, intervalWidth) map {
        case ((a, b), n) => s"[${a.toString.take(4)}, ${b.toString.take(4)}) : $n"
      } mkString ("\n")
    }
  }

}
