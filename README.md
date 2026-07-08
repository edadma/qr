# qr

![Maven Central](https://img.shields.io/maven-central/v/io.github.edadma/qr_sjs1_3)
[![Last Commit](https://img.shields.io/github/last-commit/edadma/qr)](https://github.com/edadma/qr/commits)
![GitHub](https://img.shields.io/github/license/edadma/qr)
![Scala Version](https://img.shields.io/badge/Scala-3.8.4-blue.svg)

A pure-Scala, dependency-free **QR Code generator**, cross-published for the JVM, JavaScript (Scala.js), and Native (Scala Native).

It is a faithful port of [Project Nayuki's QR Code generator library](https://www.nayuki.io/page/qr-code-generator-library) (MIT), which makes it a well-tested reference for the parts that are easy to get wrong: Reed–Solomon error correction over GF(2⁸), data masking with the four penalty rules, and version/character-count selection.

## Features

- All 40 versions (21×21 up to 177×177 modules) and all four error-correction levels (L / M / Q / H).
- Automatic mode selection (numeric / alphanumeric / byte), automatic version selection, automatic optimal masking, and optional error-correction boosting.
- Zero runtime dependencies; the core is pure Scala with no `java.*` or platform APIs, so it runs identically on JVM, JS, and Native.
- A raw boolean module matrix (draw it however you like) plus a self-contained `toSvg` convenience.

## Usage

```scala
import io.github.edadma.qr.*

val qr = Qr.encode("https://example.com", Ecc.Quartile)

// Read the module grid directly (true = dark):
for y <- 0 until qr.size do
  println((0 until qr.size).map(x => if qr(x, y) then "██" else "  ").mkString)

// Or emit an SVG string:
val svg: String = qr.toSvg(scale = 8, border = 4, dark = "#000000", light = "#ffffff")
```

For finer control — a fixed version range, an explicit mask, raw bytes, or pre-built segments — use the factory methods on `QrCode`:

```scala
QrCode.encodeText("HELLO", Ecc.High, minVersion = 1, maxVersion = 10)
QrCode.encodeBinary(bytes, Ecc.Medium)
QrCode.encodeSegments(QrSegment.makeSegments("12345"), Ecc.Low)
```

## API

- **`Qr.encode(text, ecc = Ecc.Medium): QrCode`** — the common entry point.
- **`QrCode`** — `size: Int`, `version: Int`, `mask: Int`, `module(x, y): Boolean` / `apply(x, y)`, `toSvg(scale, border, dark, light): String`.
- **`Ecc`** — `Low`, `Medium`, `Quartile`, `High`.
- **`QrSegment`** — `makeSegments`, `makeNumeric`, `makeAlphanumeric`, `makeBytes` for building segment lists by hand.

## Building

```bash
sbt qrJVM/test      # JVM
sbt qrJS/test       # JavaScript (Node.js)
sbt qrNative/test   # Native (LLVM/Clang)
```

## License

MIT — see [LICENSE](LICENSE). This library is a derivative of Project Nayuki's MIT-licensed QR Code generator library; his copyright notice is retained in the license file.
