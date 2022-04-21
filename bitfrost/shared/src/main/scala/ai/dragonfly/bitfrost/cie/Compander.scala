package ai.dragonfly.bitfrost.cie

import ai.dragonfly.math.vector.Vector3


trait Compander {
  def decode(V: Double): Double

  def encode(v: Double): Double
}

case class Gamma(gamma: Double) extends Compander {
  val Γ: Double = gamma
  val `1/Γ`: Double = 1.0 / gamma

  override inline def decode(V: Double): Double = Math.pow(V, gamma)

  override inline def encode(v: Double): Double = Math.pow(v, `1/Γ`)
}

object SRGB extends Compander {
  val `1/2.4`: Double = 0.416666667 //1.0 / 2.4

  override inline def decode(V: Double): Double = if (V > 0.04045) {
    Math.pow((0.947867299 * V) + 0.052132701, 2.4) // Math.pow((V + 0.055) / 1.055, 2.4)
  } else {
    V / 12.92
  }

  override inline def encode(v: Double): Double = if (v > 0.0031308) {
    Math.pow(1.055 * v, `1/2.4`) - 0.055
  } else {
    12.92 * v
  }
}


object Lstar extends Compander {
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