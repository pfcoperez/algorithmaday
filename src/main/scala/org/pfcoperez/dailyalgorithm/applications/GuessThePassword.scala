package org.pfcoperez.dailyalgorithm.applications

import org.pfcoperez.dailyalgorithm.Combinatorics.bruteForce

object GuessThePassword extends App {

  def dictionaryAttack(passPhraseLength: Int)(
    dictionary: Seq[String],
    testFunction: String => Boolean,
    wordGenHeuristics: Seq[String => Seq[String]] = Seq.empty): Seq[String] = {

    val words = (
      for (word: String <- dictionary.toSet; heuristic <- wordGenHeuristics; generated <- heuristic(word)) yield generated) toList

    bruteForce[String, String]("")(soFar => words filter (_.length + soFar.length < passPhraseLength))(testFunction) {
      (soFar, newWord) => soFar + newWord
    }

  }

  // Our target is an astrophysicist and ...
  val planets = List("mercury", "venus", "earth", "mars", "jupiter", "saturn", "uranus", "neptune")
  // ...and he/she has 2 childs with ages ranging from 1 to 6 years.
  val years = (2000 to 2015) map (_ toString) toList

  // Here we have an heuristic: He/she might had added upper cases to the password
  val upperLowerCase: String => Seq[String] = { str =>
    if (str.toUpperCase == str.toLowerCase) Seq(str)
    else {
      val indexedStr = str zipWithIndex;
      for (j <- 0 to str.length; i <- 0 to j)
        yield (indexedStr map { case (c, idx) => if (i <= idx && idx <= j) c.toUpper else c.toLower }) mkString
    }
  }

  val passwords = dictionaryAttack(20)(planets ::: years, _ == "jupiterSaturn2015", upperLowerCase :: Nil)
  println(passwords)

}
