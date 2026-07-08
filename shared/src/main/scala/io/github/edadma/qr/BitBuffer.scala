package io.github.edadma.qr

import scala.collection.mutable.ArrayBuffer

/** An appendable sequence of bits (0s and 1s), used to accumulate the data bit
  * stream while building a QR Code. The bits are stored most-significant-first
  * as they are appended.
  */
final class BitBuffer:
  private val bits = ArrayBuffer[Boolean]()

  /** The number of bits currently in the buffer. */
  def bitLength: Int = bits.length

  /** Returns bit `i` (0 or 1); `i` must be in `[0, bitLength)`. */
  def getBit(i: Int): Int = if bits(i) then 1 else 0

  /** Appends the `len` low-order bits of `value`, most significant first.
    * `value` must be non-negative and representable in `len` bits.
    */
  def appendBits(value: Int, len: Int): Unit =
    require(len >= 0 && len <= 31 && (value >>> len) == 0, "Value out of range")
    var i = len - 1
    while i >= 0 do
      bits += ((value >>> i) & 1) != 0
      i -= 1

  /** Appends every bit of `other`, in order. */
  def appendData(other: BitBuffer): Unit = bits ++= other.bits

  /** Returns an independent copy of this buffer. */
  def cloneBuffer: BitBuffer =
    val bb = new BitBuffer
    bb.bits ++= bits
    bb
