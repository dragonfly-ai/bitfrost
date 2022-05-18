package ai.dragonfly.bitfrost.cie

import ai.dragonfly.math.vector.Vector3
import ai.dragonfly.bitfrost.cie.Constant.*


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

object sRGB_Constants {
  val `1/2.4`: Double = 0.4166666666666667
  val `1/1.055`:Double = 0.9478672985781991
  val `1/12.9232102`: Double = 0.077380154352051
}

object sRGB_ICC_V2 extends TransferFunction {
  import sRGB_Constants.*
  // https://en.wikipedia.org/wiki/SRGB#From_sRGB_to_CIE_XYZ

  override inline def decode(i: Double): Double = if (i > 0.04045) {
    Math.pow(`1/1.055` * (i + 0.055), 2.4) // Math.pow((V + 0.055) / 1.055, 2.4)
  } else {
    `1/12.9232102` * i
  }


  override inline def encode(v: Double): Double = if (v > 0.0031308) {
    1.055 * Math.pow(v, `1/2.4`) - 0.055
  } else {
    12.9232102 * v
  }

}

//
//object sRGB_ICC_V4 extends TransferFunction {
//  import sRGB_Constants.*
//  // https://www.color.org/sRGB.pdf
////  val `(79.8 / 12.9232102) / 80.0`:Double = 0.07718670396617087
////  //val `1/((79.8 / 12.9232102) / 80.0)`:Double = 12.955599197994989
////
////  val `80/79.8`:Double = 1.0025062656641603
////  val `0.2/79.8`:Double = 0.0025062656641604013
////
////  val A:Double = 0.8772169227486248 // (79.8 / Math.pow(1.055, 2.4)) / 80.0
////
////  override inline def decode(i: Double): Double = if (i > 0.04045) {
////    A * Math.pow(i + 0.055, 2.4)
////  } else {
////    (`(79.8 / 12.9232102) / 80.0` * i) + 0.0025
////  }
////
////  override inline def encode(i: Double): Double = if (i > 0.0031308) {
////    (1.055 * Math.pow((`80/79.8` * i) - `0.2/79.8`, `1/2.4`)) - 0.055
////  } else {
////    //`1/((79.8 / 12.9232102) / 80.0)` * (i - 0.0025)
////    12.955599197994987 * (i - 0.0025)
////  }
//
//  override inline def decode(i: Double): Double = if (i > 0.04045) {
//    Math.pow(0.946879 * i + 0.0520784, 2.4) + 0.0025
//  } else {
//    (0.0772059 * i) + 0.0025
//  }
//
//  override inline def encode(i: Double): Double = sRGB_ICC_V2.encode(i)
////  if (i > 0.0031308) {
////    (Math.pow(i - 0.0025, `1/2.4`) - 0.0520784) / 0.0946879
////  } else {
////    (i - 0.0025) / 0.0772059
////  }
//
//}

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