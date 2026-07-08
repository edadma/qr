package io.github.edadma.qr

/** The error-correction level in a QR Code symbol. Higher levels tolerate more
  * damage to the printed code at the cost of storing less data per version.
  *
  * @param ordinal    the level's index 0..3 (unused externally, kept for parity)
  * @param formatBits the 2-bit value written into the symbol's format info
  */
enum Ecc(val formatBits: Int):
  /** Tolerates about 7% erroneous codewords. */
  case Low extends Ecc(1)
  /** Tolerates about 15% erroneous codewords. */
  case Medium extends Ecc(0)
  /** Tolerates about 25% erroneous codewords. */
  case Quartile extends Ecc(3)
  /** Tolerates about 30% erroneous codewords. */
  case High extends Ecc(2)
