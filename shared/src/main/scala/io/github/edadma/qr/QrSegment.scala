package io.github.edadma.qr

/** A segment of character/binary/control data in a QR Code symbol. Each segment
  * has an encoding mode and a stream of already-encoded data bits. Segments are
  * produced by the factory methods on the companion object and consumed by
  * [[QrCode.encodeSegments]].
  *
  * @param mode     the encoding mode of this segment
  * @param numChars the character count (its meaning depends on the mode)
  * @param data     the encoded bits (its length need not be a multiple of 8)
  */
final class QrSegment private[qr] (
    val mode: QrSegment.Mode,
    val numChars: Int,
    private[qr] val data: BitBuffer,
):
  require(numChars >= 0, "Invalid value")

  private[qr] def bitLength: Int = data.bitLength

object QrSegment:

  /** A QR Code encoding mode: numeric, alphanumeric, byte, or kanji. The
    * `modeBits` value is written into the symbol; the three
    * character-count-field widths depend on the symbol version group.
    */
  enum Mode(val modeBits: Int, private[qr] val ccBits: Array[Int]):
    case Numeric      extends Mode(0x1, Array(10, 12, 14))
    case Alphanumeric extends Mode(0x2, Array(9, 11, 13))
    case Byte         extends Mode(0x4, Array(8, 16, 16))
    case Kanji        extends Mode(0x8, Array(8, 10, 12))

    /** The bit width of the character-count field for a symbol of the given
      * version (1..40).
      */
    private[qr] def numCharCountBits(ver: Int): Int =
      ccBits((ver + 7) / 17)

  /** The characters encodable in alphanumeric mode, indexed by their values. */
  private val AlphanumericCharset =
    "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ $%*+-./:"

  /** Returns a segment encoding the given string in byte mode, interpreting the
    * string as UTF-8. This mode encodes any text at 8 bits per byte.
    */
  def makeBytes(data: Array[Byte]): QrSegment =
    val bb = new BitBuffer
    for b <- data do bb.appendBits(b & 0xff, 8)
    new QrSegment(Mode.Byte, data.length, bb)

  /** Returns a segment encoding the given decimal-digit string in numeric mode.
    * All characters must be in `0`..`9`.
    */
  def makeNumeric(digits: String): QrSegment =
    require(digits.forall(c => c >= '0' && c <= '9'), "String contains non-numeric characters")
    val bb = new BitBuffer
    var i = 0
    while i < digits.length do
      val n = math.min(digits.length - i, 3)
      bb.appendBits(digits.substring(i, i + n).toInt, n * 3 + 1)
      i += n
    new QrSegment(Mode.Numeric, digits.length, bb)

  /** Returns a segment encoding the given string in alphanumeric mode. Every
    * character must belong to [[isAlphanumeric]]'s 45-character set.
    */
  def makeAlphanumeric(text: String): QrSegment =
    require(isAlphanumeric(text), "String contains unencodable characters in alphanumeric mode")
    val bb = new BitBuffer
    var i = 0
    while i + 2 <= text.length do
      val v = AlphanumericCharset.indexOf(text(i)) * 45 + AlphanumericCharset.indexOf(text(i + 1))
      bb.appendBits(v, 11)
      i += 2
    if i < text.length then bb.appendBits(AlphanumericCharset.indexOf(text(i)), 6)
    new QrSegment(Mode.Alphanumeric, text.length, bb)

  /** Returns a list of segments encoding `text` in the most compact way the
    * simple heuristic allows: one numeric, alphanumeric, or byte segment.
    */
  def makeSegments(text: String): List[QrSegment] =
    if text.isEmpty then Nil
    else if isNumeric(text) then List(makeNumeric(text))
    else if isAlphanumeric(text) then List(makeAlphanumeric(text))
    else List(makeBytes(text.getBytes("UTF-8")))

  /** Tests whether every character of `text` is a decimal digit. */
  def isNumeric(text: String): Boolean =
    text.forall(c => c >= '0' && c <= '9')

  /** Tests whether every character of `text` is in the alphanumeric charset. */
  def isAlphanumeric(text: String): Boolean =
    text.forall(AlphanumericCharset.indexOf(_) >= 0)

  /** The total number of bits a list of segments occupies in a symbol of the
    * given version, or -1 if any character-count field would overflow.
    */
  private[qr] def getTotalBits(segs: List[QrSegment], version: Int): Long =
    var result = 0L
    var overflow = false
    for seg <- segs if !overflow do
      val ccbits = seg.mode.numCharCountBits(version)
      if seg.numChars >= (1 << ccbits) then overflow = true // count field would not fit
      else
        result += 4L + ccbits + seg.bitLength
        if result > Int.MaxValue then overflow = true
    if overflow then -1 else result
