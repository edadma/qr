package io.github.edadma.qr

import scala.collection.mutable.ArrayBuffer

/** A generated QR Code symbol: an immutable square grid of dark and light
  * modules. Instances are produced by the factory methods here (or the
  * convenience [[Qr]] object) and rendered via [[module]] / [[toSvg]].
  *
  * @param version               the symbol version, 1..40 (grid size grows with it)
  * @param errorCorrectionLevel  the error-correction level actually used
  * @param dataCodewords         the data codeword bytes to encode (after padding)
  * @param msk                   the mask pattern to apply, 0..7, or -1 to auto-select
  */
final class QrCode private (
    val version: Int,
    val errorCorrectionLevel: Ecc,
    dataCodewords: Array[Byte],
    private var msk: Int,
):
  import QrCode.*

  require(version >= MinVersion && version <= MaxVersion, "Version value out of range")
  require(msk >= -1 && msk <= 7, "Mask value out of range")

  /** The width and height of this symbol, in modules: `version * 4 + 17`. */
  val size: Int = version * 4 + 17

  /** The mask pattern used, 0..7 (resolved even if auto-selected). */
  def mask: Int = msk

  private val modules    = Array.ofDim[Boolean](size, size)
  private val isFunction = Array.ofDim[Boolean](size, size)

  drawFunctionPatterns()
  private val allCodewords = addEccAndInterleave(dataCodewords)
  drawCodewords(allCodewords)

  if msk == -1 then
    var minPenalty = Int.MaxValue
    for i <- 0 until 8 do
      applyMask(i)
      drawFormatBits(i)
      val penalty = getPenaltyScore
      if penalty < minPenalty then
        msk = i
        minPenalty = penalty
      applyMask(i) // XOR is its own inverse; undo before trying the next mask
  applyMask(msk)
  drawFormatBits(msk)

  /** Returns the colour of the module at `(x, y)`: `true` for dark, `false` for
    * light. Coordinates outside `[0, size)` return `false` (light), so callers
    * may render a quiet-zone border without bounds checks.
    */
  def module(x: Int, y: Int): Boolean =
    0 <= x && x < size && 0 <= y && y < size && modules(y)(x)

  /** Alias for [[module]], enabling `qr(x, y)` call syntax. */
  def apply(x: Int, y: Int): Boolean = module(x, y)

  /** Renders this symbol as a self-contained SVG document string.
    *
    * @param scale  the edge length of one module, in SVG user units (> 0)
    * @param border the quiet-zone width around the symbol, in modules (>= 0)
    * @param dark   the dark-module colour (any CSS colour, default black)
    * @param light  the light background colour (any CSS colour, default white)
    */
  def toSvg(scale: Int = 10, border: Int = 4, dark: String = "#000000", light: String = "#ffffff"): String =
    require(scale > 0, "Scale must be positive")
    require(border >= 0, "Border must be non-negative")
    val dim = (size + border * 2) * scale
    val sb  = new StringBuilder
    sb.append("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n")
    sb.append(
      s"""<svg xmlns="http://www.w3.org/2000/svg" version="1.1" viewBox="0 0 $dim $dim" stroke="none">\n""",
    )
    sb.append(s"""\t<rect width="100%" height="100%" fill="$light"/>\n""")
    sb.append("\t<path d=\"")
    var first = true
    for y <- 0 until size; x <- 0 until size if modules(y)(x) do
      if !first then sb.append(' ')
      val px = (x + border) * scale
      val py = (y + border) * scale
      sb.append(s"M$px,${py}h${scale}v${scale}h-${scale}z")
      first = false
    sb.append(s"""" fill="$dark"/>\n""")
    sb.append("</svg>\n")
    sb.toString

  /** Draws the timing, finder, alignment, format-info, and version-info
    * patterns — everything that is not data — marking each as a function module.
    */
  private def drawFunctionPatterns(): Unit =
    for i <- 0 until size do
      setFunctionModule(6, i, i % 2 == 0)
      setFunctionModule(i, 6, i % 2 == 0)
    drawFinderPattern(3, 3)
    drawFinderPattern(size - 4, 3)
    drawFinderPattern(3, size - 4)
    val alignPatPos = getAlignmentPatternPositions
    val numAlign    = alignPatPos.length
    for i <- 0 until numAlign; j <- 0 until numAlign do
      if !((i == 0 && j == 0) || (i == 0 && j == numAlign - 1) || (i == numAlign - 1 && j == 0)) then
        drawAlignmentPattern(alignPatPos(i), alignPatPos(j))
    drawFormatBits(0)
    drawVersion()

  /** Draws the two copies of the 15-bit format information (error-correction
    * level combined with `msk`) around the finder patterns.
    */
  private def drawFormatBits(mask: Int): Unit =
    val dataVal = errorCorrectionLevel.formatBits << 3 | mask
    var rem     = dataVal
    for _ <- 0 until 10 do rem = (rem << 1) ^ ((rem >>> 9) * 0x537)
    val bits = (dataVal << 10 | rem) ^ 0x5412
    assert(bits >>> 15 == 0)

    for i <- 0 to 5 do setFunctionModule(8, i, getBit(bits, i))
    setFunctionModule(8, 7, getBit(bits, 6))
    setFunctionModule(8, 8, getBit(bits, 7))
    setFunctionModule(7, 8, getBit(bits, 8))
    for i <- 9 until 15 do setFunctionModule(14 - i, 8, getBit(bits, i))

    for i <- 0 until 8 do setFunctionModule(size - 1 - i, 8, getBit(bits, i))
    for i <- 8 until 15 do setFunctionModule(8, size - 15 + i, getBit(bits, i))
    setFunctionModule(8, size - 8, true) // Always dark

  /** Draws the two copies of the 18-bit version information, present only for
    * symbols of version 7 and above.
    */
  private def drawVersion(): Unit =
    if version < 7 then ()
    else
      var rem = version
      for _ <- 0 until 12 do rem = (rem << 1) ^ ((rem >>> 11) * 0x1f25)
      val bits = version << 12 | rem
      assert(bits >>> 18 == 0)
      for i <- 0 until 18 do
        val bit = getBit(bits, i)
        val a   = size - 11 + i % 3
        val b   = i / 3
        setFunctionModule(a, b, bit)
        setFunctionModule(b, a, bit)

  /** Draws a 9x9 finder pattern (including the one-module separator) centred at
    * `(x, y)`, clipping any part that would fall outside the grid.
    */
  private def drawFinderPattern(x: Int, y: Int): Unit =
    for dy <- -4 to 4; dx <- -4 to 4 do
      val dist = math.max(math.abs(dx), math.abs(dy)) // Chebyshev/infinity norm
      val xx   = x + dx
      val yy   = y + dy
      if 0 <= xx && xx < size && 0 <= yy && yy < size then
        setFunctionModule(xx, yy, dist != 2 && dist != 4)

  /** Draws a 5x5 alignment pattern centred at `(x, y)`; the centre coordinates
    * always leave the whole pattern inside the grid.
    */
  private def drawAlignmentPattern(x: Int, y: Int): Unit =
    for dy <- -2 to 2; dx <- -2 to 2 do
      setFunctionModule(x + dx, y + dy, math.max(math.abs(dx), math.abs(dy)) != 1)

  /** Sets the module at `(x, y)` to the given colour and records it as a
    * function (non-data) module.
    */
  private def setFunctionModule(x: Int, y: Int, isDark: Boolean): Unit =
    modules(y)(x) = isDark
    isFunction(y)(x) = true

  /** Appends error-correction codewords to `data`, splits everything into the
    * per-version blocks, and interleaves them into the final codeword sequence
    * that [[drawCodewords]] lays into the grid.
    */
  private def addEccAndInterleave(data: Array[Byte]): Array[Byte] =
    require(data.length == getNumDataCodewords(version, errorCorrectionLevel), "Illegal argument")

    val numBlocks     = NumErrorCorrectionBlocks(errorCorrectionLevel.ordinal)(version)
    val blockEccLen   = EccCodewordsPerBlock(errorCorrectionLevel.ordinal)(version)
    val rawCodewords  = getNumRawDataModules(version) / 8
    val numShortBlocks = numBlocks - rawCodewords % numBlocks
    val shortBlockLen  = rawCodewords / numBlocks

    val blocks  = new ArrayBuffer[Array[Byte]](numBlocks)
    val rsDiv   = reedSolomonComputeDivisor(blockEccLen)
    var k       = 0
    for i <- 0 until numBlocks do
      val datLen = shortBlockLen - blockEccLen + (if i < numShortBlocks then 0 else 1)
      val dat    = data.slice(k, k + datLen)
      k += datLen
      val block = Array.ofDim[Byte](shortBlockLen + 1)
      System.arraycopy(dat, 0, block, 0, dat.length)
      val ecc = reedSolomonComputeRemainder(dat, rsDiv)
      System.arraycopy(ecc, 0, block, block.length - blockEccLen, ecc.length)
      blocks += block

    val result = new ArrayBuffer[Byte](rawCodewords)
    for i <- 0 until blocks(0).length do
      for j <- blocks.indices do
        // Every block contributes at each column except the padding cell that
        // the short blocks lack in the data region.
        if i != shortBlockLen - blockEccLen || j >= numShortBlocks then
          result += blocks(j)(i)
    result.toArray

  /** Lays the interleaved codewords into the grid in the zig-zag column order,
    * skipping function modules; leftover bits (always zero) fill any remainder.
    */
  private def drawCodewords(data: Array[Byte]): Unit =
    require(data.length == getNumRawDataModules(version) / 8, "Illegal argument")
    var i = 0 // bit index into data
    var right = size - 1
    while right >= 1 do
      if right == 6 then right = 5 // the vertical timing pattern occupies column 6
      for vert <- 0 until size do
        for j <- 0 until 2 do
          val x      = right - j
          val upward = ((right + 1) & 2) == 0
          val y      = if upward then size - 1 - vert else vert
          if !isFunction(y)(x) && i < data.length * 8 then
            modules(y)(x) = getBit(data(i >>> 3).toInt & 0xff, 7 - (i & 7))
            i += 1
      right -= 2
    assert(i == data.length * 8)

  /** XORs one of the eight mask patterns over every non-function module. XOR is
    * self-inverse, so calling this twice with the same `mask` is a no-op.
    */
  private def applyMask(mask: Int): Unit =
    require(mask >= 0 && mask <= 7, "Mask value out of range")
    for y <- 0 until size; x <- 0 until size do
      val invert = mask match
        case 0 => (x + y) % 2 == 0
        case 1 => y % 2 == 0
        case 2 => x % 3 == 0
        case 3 => (x + y) % 3 == 0
        case 4 => (x / 3 + y / 2) % 2 == 0
        case 5 => x * y % 2 + x * y % 3 == 0
        case 6 => (x * y % 2 + x * y % 3) % 2 == 0
        case 7 => ((x + y) % 2 + x * y % 3) % 2 == 0
      if !isFunction(y)(x) && invert then modules(y)(x) = !modules(y)(x)

  /** Computes the total penalty score of the current grid under the four
    * standard rules (runs, 2x2 blocks, finder-like patterns, dark ratio); used
    * to pick the mask that gives the most robust symbol.
    */
  private def getPenaltyScore: Int =
    var result = 0

    for y <- 0 until size do
      var runColor  = false
      var runX      = 0
      val runHistory = Array.fill(7)(0)
      for x <- 0 until size do
        if modules(y)(x) == runColor then
          runX += 1
          if runX == 5 then result += PenaltyN1
          else if runX > 5 then result += 1
        else
          finderPenaltyAddHistory(runX, runHistory)
          if !runColor then result += finderPenaltyCountPatterns(runHistory) * PenaltyN3
          runColor = modules(y)(x)
          runX = 1
      result += finderPenaltyTerminateAndCount(runColor, runX, runHistory) * PenaltyN3

    for x <- 0 until size do
      var runColor  = false
      var runY      = 0
      val runHistory = Array.fill(7)(0)
      for y <- 0 until size do
        if modules(y)(x) == runColor then
          runY += 1
          if runY == 5 then result += PenaltyN1
          else if runY > 5 then result += 1
        else
          finderPenaltyAddHistory(runY, runHistory)
          if !runColor then result += finderPenaltyCountPatterns(runHistory) * PenaltyN3
          runColor = modules(y)(x)
          runY = 1
      result += finderPenaltyTerminateAndCount(runColor, runY, runHistory) * PenaltyN3

    for y <- 0 until size - 1; x <- 0 until size - 1 do
      val c = modules(y)(x)
      if c == modules(y)(x + 1) && c == modules(y + 1)(x) && c == modules(y + 1)(x + 1) then
        result += PenaltyN2

    var dark = 0
    for row <- modules; cell <- row if cell do dark += 1
    val total = size * size
    // Smallest k such that 20% <= (50 - 10k)% <= dark/total <= (50 + 10k)% <= 80%.
    val k = (math.abs(dark * 20 - total * 10) + total - 1) / total - 1
    assert(0 <= k && k <= 9)
    result += k * PenaltyN4
    assert(0 <= result && result <= 2568888)
    result

  /** The row/column coordinates of the alignment-pattern centres for this
    * version. Version 1 has none; higher versions space them evenly.
    */
  private def getAlignmentPatternPositions: Array[Int] =
    if version == 1 then Array.empty
    else
      val numAlign = version / 7 + 2
      val step     = (version * 8 + numAlign * 3 + 5) / (numAlign * 4 - 4) * 2
      val result   = Array.ofDim[Int](numAlign)
      result(0) = 6
      var pos = size - 7
      var i   = numAlign - 1
      while i >= 1 do
        result(i) = pos
        pos -= step
        i -= 1
      result

  /** Counts how many finder-like patterns (the 1:1:3:1:1 dark/light ratio with a
    * wide light margin) end at the current position, per the standard's rule 3.
    */
  private def finderPenaltyCountPatterns(runHistory: Array[Int]): Int =
    val n = runHistory(1)
    assert(n <= size * 3)
    val core = n > 0 && runHistory(2) == n && runHistory(3) == n * 3 && runHistory(4) == n && runHistory(5) == n
    (if core && runHistory(0) >= n * 4 && runHistory(6) >= n then 1 else 0) +
      (if core && runHistory(6) >= n * 4 && runHistory(0) >= n then 1 else 0)

  /** Terminates a row/column scan for rule 3: flushes the final run (padding
    * with an implicit light margin if the line ended dark) and counts patterns.
    */
  private def finderPenaltyTerminateAndCount(currentRunColor: Boolean, run: Int, runHistory: Array[Int]): Int =
    var currentRunLength = run
    if currentRunColor then
      finderPenaltyAddHistory(currentRunLength, runHistory)
      currentRunLength = 0
    finderPenaltyAddHistory(currentRunLength + size, runHistory)
    finderPenaltyCountPatterns(runHistory)

  /** Pushes the length of the just-ended run onto the front of the seven-entry
    * run-length history used by rule 3.
    */
  private def finderPenaltyAddHistory(currentRunLength: Int, runHistory: Array[Int]): Unit =
    var len = currentRunLength
    if runHistory(0) == 0 then len += size // add light border to initial run
    System.arraycopy(runHistory, 0, runHistory, 1, runHistory.length - 1)
    runHistory(0) = len

object QrCode:

  /** The smallest supported symbol version. */
  val MinVersion: Int = 1
  /** The largest supported symbol version. */
  val MaxVersion: Int = 40

  private val PenaltyN1 = 3
  private val PenaltyN2 = 3
  private val PenaltyN3 = 40
  private val PenaltyN4 = 10

  private val EccCodewordsPerBlock: Array[Array[Int]] = Array(
    // Version: (note that index 0 is for padding, and is set to an illegal value)
    Array(-1, 7, 10, 15, 20, 26, 18, 20, 24, 30, 18, 20, 24, 26, 30, 22, 24, 28, 30, 28, 28, 28, 28, 30, 30, 26, 28, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30), // Low
    Array(-1, 10, 16, 26, 18, 24, 16, 18, 22, 22, 26, 30, 22, 22, 24, 24, 28, 28, 26, 26, 26, 26, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28), // Medium
    Array(-1, 13, 22, 18, 26, 18, 24, 18, 22, 20, 24, 28, 26, 24, 20, 30, 24, 28, 28, 26, 30, 28, 30, 30, 30, 30, 28, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30), // Quartile
    Array(-1, 17, 28, 22, 16, 22, 28, 26, 26, 24, 28, 24, 28, 22, 24, 24, 30, 28, 28, 26, 28, 30, 24, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30), // High
  )

  private val NumErrorCorrectionBlocks: Array[Array[Int]] = Array(
    // Version: (note that index 0 is for padding, and is set to an illegal value)
    Array(-1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 4, 4, 4, 4, 4, 6, 6, 6, 6, 7, 8, 8, 9, 9, 10, 12, 12, 12, 13, 14, 15, 16, 17, 18, 19, 19, 20, 21, 22, 24, 25),   // Low
    Array(-1, 1, 1, 1, 2, 2, 4, 4, 4, 5, 5, 5, 8, 9, 9, 10, 10, 11, 13, 14, 16, 17, 17, 18, 20, 21, 23, 25, 26, 28, 29, 31, 33, 35, 37, 38, 40, 43, 45, 47, 49),  // Medium
    Array(-1, 1, 1, 2, 2, 4, 4, 6, 6, 8, 8, 8, 10, 12, 16, 12, 17, 16, 18, 21, 20, 23, 23, 25, 27, 29, 34, 34, 35, 38, 40, 43, 45, 48, 51, 53, 56, 59, 62, 65, 68), // Quartile
    Array(-1, 1, 1, 2, 4, 4, 4, 5, 6, 8, 8, 11, 11, 16, 16, 18, 16, 19, 21, 25, 25, 25, 34, 30, 32, 35, 37, 40, 42, 45, 48, 51, 54, 57, 60, 63, 66, 70, 74, 77, 81), // High
  )

  /** Returns the colour, `true` for 1, of bit `i` of `x`. */
  private def getBit(x: Int, i: Int): Boolean = ((x >>> i) & 1) != 0

  /** Encodes `text` (auto-selecting the segment mode) into the smallest symbol
    * whose version lies in `[minVersion, maxVersion]` and that fits the data at
    * `ecl`. If `boostEcl` is set, the level is silently raised to the strongest
    * one the chosen version still accommodates.
    */
  def encodeText(
      text: String,
      ecl: Ecc,
      minVersion: Int = MinVersion,
      maxVersion: Int = MaxVersion,
      mask: Int = -1,
      boostEcl: Boolean = true,
  ): QrCode =
    encodeSegments(QrSegment.makeSegments(text), ecl, minVersion, maxVersion, mask, boostEcl)

  /** Encodes raw bytes in byte mode. See [[encodeText]] for the version/ECC
    * arguments.
    */
  def encodeBinary(
      data: Array[Byte],
      ecl: Ecc,
      minVersion: Int = MinVersion,
      maxVersion: Int = MaxVersion,
      mask: Int = -1,
      boostEcl: Boolean = true,
  ): QrCode =
    encodeSegments(List(QrSegment.makeBytes(data)), ecl, minVersion, maxVersion, mask, boostEcl)

  /** Encodes a prepared list of segments. Chooses the smallest fitting version,
    * optionally boosts the error-correction level, appends the terminator and
    * padding codewords, and builds the symbol.
    */
  def encodeSegments(
      segs: List[QrSegment],
      ecl0: Ecc,
      minVersion: Int = MinVersion,
      maxVersion: Int = MaxVersion,
      mask: Int = -1,
      boostEcl: Boolean = true,
  ): QrCode =
    require(
      MinVersion <= minVersion && minVersion <= maxVersion && maxVersion <= MaxVersion,
      "Invalid version range",
    )
    require(-1 <= mask && mask <= 7, "Mask value out of range")

    var ecl = ecl0

    var version  = minVersion
    var dataUsedBits = 0L
    var found    = false
    while !found do
      val dataCapacityBits = getNumDataCodewords(version, ecl) * 8
      val used             = QrSegment.getTotalBits(segs, version)
      if used != -1 && used <= dataCapacityBits then
        dataUsedBits = used
        found = true
      else if version >= maxVersion then
        val msg =
          if used == -1 then "Segment too long"
          else s"Data length = $used bits, Max capacity = $dataCapacityBits bits"
        throw new IllegalArgumentException(s"Data too long. $msg")
      else version += 1

    for newEcl <- Seq(Ecc.Medium, Ecc.Quartile, Ecc.High) do
      if boostEcl && dataUsedBits <= getNumDataCodewords(version, newEcl) * 8L then ecl = newEcl

    val bb = new BitBuffer
    for seg <- segs do
      bb.appendBits(seg.mode.modeBits, 4)
      bb.appendBits(seg.numChars, seg.mode.numCharCountBits(version))
      bb.appendData(seg.data)
    assert(bb.bitLength == dataUsedBits)

    val dataCapacityBits = getNumDataCodewords(version, ecl) * 8
    assert(bb.bitLength <= dataCapacityBits)
    bb.appendBits(0, math.min(4, dataCapacityBits - bb.bitLength))
    bb.appendBits(0, (8 - bb.bitLength % 8) % 8)
    assert(bb.bitLength % 8 == 0)

    var padByte = 0xec
    while bb.bitLength < dataCapacityBits do
      bb.appendBits(padByte, 8)
      padByte ^= 0xec ^ 0x11

    val dataCodewords = Array.ofDim[Byte](bb.bitLength / 8)
    for i <- 0 until bb.bitLength do
      dataCodewords(i >>> 3) = (dataCodewords(i >>> 3) | (bb.getBit(i) << (7 - (i & 7)))).toByte

    new QrCode(version, ecl, dataCodewords, mask)

  /** The number of 8-bit data codewords (not counting ECC) that a symbol of the
    * given version and error-correction level can store.
    */
  private[qr] def getNumDataCodewords(ver: Int, ecl: Ecc): Int =
    getNumRawDataModules(ver) / 8 -
      EccCodewordsPerBlock(ecl.ordinal)(ver) * NumErrorCorrectionBlocks(ecl.ordinal)(ver)

  /** The number of data-region modules (i.e. codeword bits before dividing by 8)
    * available in a symbol of the given version, after subtracting all function
    * patterns.
    */
  private def getNumRawDataModules(ver: Int): Int =
    require(ver >= MinVersion && ver <= MaxVersion, "Version number out of range")
    var result = (16 * ver + 128) * ver + 64
    if ver >= 2 then
      val numAlign = ver / 7 + 2
      result -= (25 * numAlign - 10) * numAlign - 55
      if ver >= 7 then result -= 36
    assert(208 <= result && result <= 29648)
    result

  /** Multiplies two elements of the GF(2^8) field (Rijndael/QR polynomial) used
    * by the Reed–Solomon coder.
    */
  private[qr] def reedSolomonMultiply(x: Int, y: Int): Int =
    var z = 0
    var i = 7
    while i >= 0 do
      z = (z << 1) ^ ((z >>> 7) * 0x11d)
      z ^= ((y >>> i) & 1) * x
      i -= 1
    assert(z >>> 8 == 0)
    z

  /** Computes the Reed–Solomon generator polynomial of the given degree, whose
    * coefficients are the divisor for remainder computation.
    */
  private[qr] def reedSolomonComputeDivisor(degree: Int): Array[Byte] =
    require(degree >= 1 && degree <= 255, "Degree out of range")
    val result = Array.ofDim[Byte](degree)
    result(degree - 1) = 1 // start with the monomial x^0
    var root = 1
    for _ <- 0 until degree do
      for j <- 0 until result.length do
        result(j) = reedSolomonMultiply(result(j) & 0xff, root).toByte
        if j + 1 < result.length then result(j) = (result(j) ^ result(j + 1)).toByte
      root = reedSolomonMultiply(root, 0x02)
    result

  /** Computes the Reed–Solomon error-correction codewords for `data` using the
    * precomputed `divisor` polynomial.
    */
  private[qr] def reedSolomonComputeRemainder(data: Array[Byte], divisor: Array[Byte]): Array[Byte] =
    val result = Array.ofDim[Byte](divisor.length)
    for b <- data do
      val factor = (b ^ result(0)) & 0xff
      System.arraycopy(result, 1, result, 0, result.length - 1)
      result(result.length - 1) = 0
      for j <- 0 until result.length do
        result(j) = (result(j) ^ reedSolomonMultiply(divisor(j) & 0xff, factor)).toByte
    result
