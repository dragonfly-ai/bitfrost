package ai.dragonfly.bitfrost.cie

import ai.dragonfly.math.vector.Vector3


/**
 * The TransferFunction handles companding.
 */

trait TransferFunction {
  def decode(V: Double): Double
  def encode(v: Double): Double
}

case class Gamma(gamma: Double) extends TransferFunction {
  val Γ: Double = gamma
  val `1/Γ`: Double = 1.0 / gamma

  override inline def decode(V: Double): Double = Math.pow(V, gamma)

  override inline def encode(v: Double): Double = Math.pow(v, `1/Γ`)
}

object sRGB_ICC_V2 extends TransferFunction {
  // https://www.color.org/sRGB.pdf
  val `1/1.055`:Double = 0.9478672985781991
  val `1/12.9232102`: Double = 0.077380154352051

  override inline def decode(V: Double): Double = if (V > 0.04045) {
    Math.pow(`1/1.055` * (V + 0.055), 2.4) // Math.pow((V + 0.055) / 1.055, 2.4)
  } else {
    `1/12.9232102` * V
  }

  val `1/2.4`: Double = 0.4166666666666667

  override inline def encode(v: Double): Double = if (v > 0.0031308) {
    Math.pow(1.055 * v, `1/2.4`) - 0.055
  } else {
    12.9232102 * v
  }
}


object sRGB_ICC_V4 extends TransferFunction {

  // https://www.color.org/sRGB.pdf
  val `(79.8 / 12.9232102) / 80.0`:Double = 0.07718670396617087
  val C:Double = 0.0025000000000000005 // (12.9232102*0.2) / (12.9232102*80) = 0.0025000000000000005

  override inline def decode(V: Double): Double = if (V > 0.04045) {
    79.8 * Math.pow(sRGB_ICC_V2.`1/1.055` * (V + 0.055), 2.4) + C// Math.pow((V + 0.055) / 1.055, 2.4)
  } else {
    (`(79.8 / 12.9232102) / 80.0` * V) + C
  }

  override inline def encode(v: Double): Double = sRGB_ICC_V2.encode(v)

}

object Lstar extends TransferFunction {
  val `1/1.16`: Double = 0.862068966 // 1.0/1.16
  val `k/100`: Double = 9.03296296296
  val `100/k`: Double = 0.110705646

  override inline def decode(V: Double): Double = if (V > 0.08) {
    Math.pow((`1/1.16` * V) + 0.137931034, 3)
  } else {
    V * `100/k`
  }

  override inline def encode(v: Double): Double = if (v > ϵ) {
    (1.16 * Math.cbrt(v)) - 0.16
  } else {
    v * `k/100`
  }
}

// todo: NTSC Video's SMPTE-170M

// todo: HD Video's SMPTE-240M