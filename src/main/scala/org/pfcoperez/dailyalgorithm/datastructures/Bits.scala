package org.pfcoperez.dailyalgorithm.datastructures

import cats.syntax.either._

import Bits._

object Bits {
  import Helper._

  def zeros(l: Long = 0L): Bits = {
    val bits =
      if (l == 0) Vector.empty[Vector[Byte]]
      else {
        val BitLocation(lastBlock, lastByte, lastBit) = locateBit(l - 1)
        Vector.fill(lastBlock)(Vector.fill(maxVectorSize.toInt)(0.toByte)) :+ Vector.fill(lastByte + 1)(0.toByte)
      }
    new Bits(l, bits)
  }

  trait Error
  object ElementNotFound extends Error
  object MaxCapacityExceeded extends Error {
    val limit: Long = maxVectorSize * maxVectorSize * 8
  }

  private type Storage = Vector[Vector[Byte]]

  private object Helper {
    type Storage = Vector[Vector[Byte]]

    case class BitLocation(block: Int, byte: Int, bit: Byte) {
      def toIndex: Long = bit + byte * maxNoBitsPerByte + block * maxNoBitsPerBlock
    }

    def locateBit(idx: Long): BitLocation = BitLocation(
      (idx / maxNoBitsPerBlock).toInt,
      ((idx % maxNoBitsPerBlock) / maxNoBitsPerByte).toInt,
      (idx % maxNoBitsPerByte).toByte)

    val maxVectorSize = Int.MaxValue.toLong / 2L

    val maxNoBytesPerBlock = maxVectorSize
    val maxNoBitsPerByte = 8
    val maxNoBitsPerBlock = maxNoBitsPerByte * maxNoBytesPerBlock
  }

}

class Bits private (val length: Long, private val bits: Storage) {
  import Helper._

  private def resultOnLocation[T](idx: Long)(f: BitLocation => T): Either[Error, T] =
    if (idx >= length) ElementNotFound.asLeft
    else f(locateBit(idx)).asRight

  /**
   * Get bit value at position `idx`
   * O(1)
   */
  def apply(idx: Long): Either[Error, Boolean] = resultOnLocation(idx) {
    case BitLocation(block, byte, bit) =>
      ((bits(block)(byte) & (0x01 << bit.toLong).toByte) > 0)
  }

  /**
   * Set bit value at position `idx`
   * O(1)
   */
  def set(idx: Long, v: Boolean = false): Either[Error, Bits] = resultOnLocation(idx) { location =>
    val BitLocation(block, byte, bit) = location
    val newByte = (bits(block)(byte) | (0x01 << bit.toLong)).toByte
    val newBlock = bits(block).updated(byte, newByte)
    new Bits(length, bits.updated(block, newBlock))
  }

  def clear(idx: Long): Either[Error, Bits] = set(idx, false)

  /**
   * Append bit
   * O(1)
   */
  def append(b: Boolean): Either[Error, Bits] =
    if (length == Long.MaxValue) MaxCapacityExceeded.asLeft
    else Right {
      val lastLocation = locateBit(length - 1)
      val newLocation = locateBit(length)
      val expandedBits: Storage =
        if (lastLocation.block != newLocation.block) bits :+ Vector[Byte](0)
        else if (lastLocation.byte != newLocation.block)
          bits.updated(bits.length - 1, bits.last :+ 0.toByte)
        else bits
      new Bits(length + 1, expandedBits)
    } flatMap (_.set(length))

}
