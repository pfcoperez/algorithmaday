package org.pfcoperez.dailyalgorithm.applications

import cats._
import cats.implicits._

object FunctionalNaiveDating extends App {

  object CandidateSelectionSystem {

    val maleCandidates: List[String] = List(
      "LeChuck",
      "Victor")

    val femaleCandidates: List[String] = List(
      "Elaine",
      "Carla",
      "Victoria")

  }

  object MatchingScores {

    /**
     * A naive match score function, it predicts the success
     * of a relationship upon how similar their names are:
     *
     * In this example imaginary world, a couple with rather different
     * names will get along better than partners with similar names
     *
     */
    def namingScore(nameA: String, nameB: String): ((String, String), Int) =
      (nameA -> nameB -> (nameA diff nameB).size)

  }

  import MatchingScores._
  import CandidateSelectionSystem._

  /**
   * A couple matching algorithm based on a score function
   * leveraging Applicative to easily apply the evaluation.
   */
  def matchScores: List[((String, String), Int)] =
    Applicative[List].pure((namingScore(_, _)).curried) ap {
      femaleCandidates.map(_.toLowerCase)
    } ap {
      maleCandidates.map(_.toLowerCase)
    }

  val bestMatch = matchScores.maxBy(_._2)
  val worstMatch = matchScores.minBy(_._2)

  println {
    val (goodNameA, goodNameB) = bestMatch._1
    val (badNameA, badNameB) = worstMatch._1
    s"""
       |$goodNameA and $goodNameB will get along
       |$badNameA and $badNameB probably not, their names are to similar,
       |that can only drive to confusion ;-P
     """.stripMargin
  }

}
