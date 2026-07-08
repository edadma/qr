package io.github.edadma.qr

/** Convenience entry point for the common case: encode a string into a QR Code
  * symbol. For control over the version range, mask, or pre-built segments, use
  * the factory methods on [[QrCode]] directly.
  */
object Qr:

  /** Encodes `text` into a [[QrCode]] at the given error-correction level
    * (default [[Ecc.Medium]]), auto-selecting the segment mode, the smallest
    * fitting version, and the optimal mask.
    */
  def encode(text: String, ecc: Ecc = Ecc.Medium): QrCode =
    QrCode.encodeText(text, ecc)
