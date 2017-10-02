package org.pfcoperez.dailyalgorithm.datastructures


import BitSeq._

object BitSeq {

  trait Bit
  object One extends Bit
  object Zero extends Bit

  trait Error
  object ElementNotFound extends Error
  object MaxCapacityExceeded extends Error {
    val limit = Long.MaxValue
  }

}

class Bits private (val length: Long, val bits: Vector[Vector[Byte]]) {

  private val noBlocks = Int.MaxValue.toLong
  private val noBitsPerBlock = Int.MaxValue.toLong*8

  case class BitLocation(block: Int, byte: Int, bit: Byte) {
    def toIndex: Long = bit + byte*8 + block*noBitsPerBlock
  }

  private def locateBit(idx: Long): BitLocation = BitLocation(
    (idx / Int.MaxValue.toLong).toInt,
    (idx % Int.MaxValue.toLong).toInt,
    (idx % 8).toByte
  )

  private def resultOnLocation[T](idx: Long)(f: BitLocation => T): Either[Error, T] =
    if(idx >= length) Left(ElementNotFound)
    else Right(f(locateBit(idx)))

  def apply(idx: Long): Either[Error, Bit] = resultOnLocation(idx) {
    case BitLocation(block, byte, bit) if((bits(block)(byte) & (0x01<<bit.toLong).toByte) > 0) =>
      One
    case _ =>
      Zero
    }
  
  def set(idx: Long, v: Bit = Zero): Either[Error, Bits] = resultOnLocation(idx) { location =>
    val BitLocation(block, byte, bit) = location
    val newByte = (bits(block)(byte) | (0x01<<bit.toLong)).toByte
    val newBlock = bits(block).updated(byte, newByte)
    new Bits(length, bits.updated(block, newBlock))
  }

  def clear(idx: Long): Either[Error, Bits] = set(idx, Zero)

  def append(b: Bit): Either[Error, Bits] =
    if(length == Long.MaxValue) Left(MaxCapacityExceeded)
    else Right {
      val lastOccupiedLocation = locateBit(length-1)
      val newLocation = locateBit(length)
      //TODO
    }

}
