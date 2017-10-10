package org.pfcoperez.dailyalgorithm.datastructures

import org.scalatest.{ FlatSpec, Matchers }

class BitsSpec extends FlatSpec with Matchers {

  def bytes2seqbool(bytes: Seq[Byte]): Seq[Boolean] =
    for {
      b <- bytes
      bit <- b.toBinaryString
    } yield bit == '1'

  def text2seqbool(str: String): Seq[Boolean] = bytes2seqbool(str.map(_.toByte))

  "A Bits (BitArray) data structure" should "be able to hold the same content as `Seq[Boolean]`" in {

    val message = """Look again at that dot. That's here. That's home. That's us. On it everyone you love, everyone you know, everyone you ever heard of, every human being who ever was, lived out their lives. The aggregate of our joy and suffering, thousands of confident religions, ideologies, and economic doctrines, every hunter and forager, every hero and coward, every creator and destroyer of civilization, every king and peasant, every young couple in love, every mother and father, hopeful child, inventor and explorer, every teacher of morals, every corrupt politician, every "superstar," every "supreme leader," every saint and sinner in the history of our species lived there-on a mote of dust suspended in a sunbeam"""

    val booleanSeq = text2seqbool(message)
    val bits = Bits(booleanSeq)

    (0L until bits.length).map(bits(_).right.get) should equal(booleanSeq)

  }

  it should "act as a monoid where empty is zero and concatenating is combine" in {

    import Bits._
    import cats.Monoid
    import cats.syntax.monoid._

    implicit def bool2bits(x: Boolean): Bits = Bits(x :: Nil)

    val bits = Monoid[Bits].empty |+| true |+| false |+| true |+| true

    (0L until bits.length).map(bits(_).right.get) should equal(Seq(true, false, true, true))

  }

}
