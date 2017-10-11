package org.pfcoperez.dailyalgorithm

import org.scalatest.{ WordSpec, Matchers }

import Sequences.HuffmanCoding._

class HuffmanSpec extends WordSpec with Matchers {

  "Huffman encoding tools" should {

    val text = """|I've seen things you people wouldn't believe. 
                  |Attack ships on fire off the shoulder of Orion. 
                  |I watched c-beams glitter in the dark near the Tannhäuser Gate. 
                  |All those moments will be lost in time, like tears in rain. 
                  |Time to die.
                  !"""

    // These means are: A symbol to binary translation table
    // and a binary to symbol translation tree.
    "Provide the means to encode and decode messages" in {

      val symbolsFrequency = Map(
        'a' -> 10,
        'e' -> 12,
        'i' -> 6,
        'o' -> 7,
        'u' -> 3)

      implicit val ac @ AlphabetCode(encode, decodingTree) = generateAlphabetCode(symbolsFrequency)

      val msg = text.toLowerCase.filter(symbolsFrequency.keySet contains _).toList
      val b = encodeMessage(msg)
      decodeMessage(b) shouldBe Right(msg)

    }

  }

}
