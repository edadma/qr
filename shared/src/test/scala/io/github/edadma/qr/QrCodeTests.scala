package io.github.edadma.qr

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class QrCodeTests extends AnyFreeSpec with Matchers:

  "GF(256) multiply matches known products" in {
    QrCode.reedSolomonMultiply(0, 5) shouldBe 0
    QrCode.reedSolomonMultiply(1, 1) shouldBe 1
    QrCode.reedSolomonMultiply(2, 2) shouldBe 4
    // alpha^1 * alpha^1 = alpha^2 in the QR field
    QrCode.reedSolomonMultiply(0x80, 0x02) shouldBe 0x1d // 0x80<<1 reduced by 0x11d
  }

  "Reed-Solomon remainder matches the ISO 18004 worked example" in {
    // The canonical example: numeric "01234567" in a Version-1-M symbol has these
    // 16 data codewords; its 10 ECC codewords are the published result.
    val data = Array[Byte](16, 32, 12, 86, 97, -128, -20, 17, -20, 17, -20, 17, -20, 17, -20, 17)
    val div  = QrCode.reedSolomonComputeDivisor(10)
    val ecc  = QrCode.reedSolomonComputeRemainder(data, div).map(_ & 0xff)
    ecc shouldBe Array(165, 36, 212, 193, 237, 54, 199, 135, 44, 85)
  }

  "capacity tables give the standard data-codeword counts" in {
    QrCode.getNumDataCodewords(1, Ecc.Low) shouldBe 19
    QrCode.getNumDataCodewords(1, Ecc.Medium) shouldBe 16
    QrCode.getNumDataCodewords(1, Ecc.Quartile) shouldBe 13
    QrCode.getNumDataCodewords(1, Ecc.High) shouldBe 9
    QrCode.getNumDataCodewords(40, Ecc.Low) shouldBe 2956
    QrCode.getNumDataCodewords(40, Ecc.High) shouldBe 1276
  }

  "size is version * 4 + 17" in {
    Qr.encode("A").size shouldBe 21
    QrCode.encodeText("A", Ecc.Low, minVersion = 7, maxVersion = 7).size shouldBe 45
    QrCode.encodeText("A", Ecc.Low, minVersion = 40, maxVersion = 40).size shouldBe 177
  }

  "version grows as data grows" in {
    Qr.encode("HELLO WORLD", Ecc.Quartile).version shouldBe 1
    val big = QrCode.encodeText("A" * 200, Ecc.Low)
    big.version should be > 1
  }

  "finder patterns sit in three corners with separators" in {
    val qr = Qr.encode("test")
    val n  = qr.size
    def finder(ox: Int, oy: Int): Unit =
      for dy <- 0 until 7; dx <- 0 until 7 do
        val ring   = dx == 0 || dx == 6 || dy == 0 || dy == 6
        val centre = dx >= 2 && dx <= 4 && dy >= 2 && dy <= 4
        qr(ox + dx, oy + dy) shouldBe (ring || centre)
    finder(0, 0)
    finder(n - 7, 0)
    finder(0, n - 7)
  }

  "timing patterns alternate along row and column 6" in {
    val qr = Qr.encode("timing")
    for i <- 8 until qr.size - 8 do
      qr(i, 6) shouldBe (i % 2 == 0)
      qr(6, i) shouldBe (i % 2 == 0)
  }

  "the fixed dark module is always set" in {
    val qr = Qr.encode("dark")
    qr(8, qr.size - 8) shouldBe true
  }

  "out-of-bounds modules read as light (quiet zone)" in {
    val qr = Qr.encode("zone")
    qr(-1, 0) shouldBe false
    qr(0, -1) shouldBe false
    qr(qr.size, 0) shouldBe false
    qr(0, qr.size) shouldBe false
  }

  "encoding is deterministic" in {
    val a = Qr.encode("repeatable", Ecc.High)
    val b = Qr.encode("repeatable", Ecc.High)
    a.size shouldBe b.size
    for y <- 0 until a.size; x <- 0 until a.size do a(x, y) shouldBe b(x, y)
  }

  "an explicit mask is honoured" in {
    val qr = QrCode.encodeText("mask", Ecc.Low, mask = 5)
    qr.mask shouldBe 5
  }

  "toSvg is well-formed and paints every dark module" in {
    val qr  = Qr.encode("svg")
    val svg = qr.toSvg(scale = 8, border = 2)
    svg should startWith("<?xml")
    svg should include("</svg>")
    svg should include("viewBox=\"0 0")
    var dark = 0
    for y <- 0 until qr.size; x <- 0 until qr.size if qr(x, y) do dark += 1
    // one "h8v8" path command per dark module
    "h8v8".r.findAllMatchIn(svg).size shouldBe dark
  }

  "numeric, alphanumeric, and byte modes are selected by content" in {
    QrSegment.makeSegments("12345").head.mode shouldBe QrSegment.Mode.Numeric
    QrSegment.makeSegments("HELLO 123").head.mode shouldBe QrSegment.Mode.Alphanumeric
    QrSegment.makeSegments("hello").head.mode shouldBe QrSegment.Mode.Byte
    QrSegment.makeSegments("café").head.mode shouldBe QrSegment.Mode.Byte
  }
