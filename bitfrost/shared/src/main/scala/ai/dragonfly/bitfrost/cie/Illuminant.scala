package ai.dragonfly.bitfrost.cie

import bridge.array.*

import ai.dragonfly.math.matrix.*
import ai.dragonfly.math.vector.*


/**
 * From: https://en.wikipedia.org/wiki/Standard_illuminant
 * "A standard illuminant is a theoretical source of visible light with a spectral power distribution that is published.
 * Standard illuminants provide a basis for comparing images or colors recorded under different lighting."
 */

object Illuminant {
  // illuminant values from: http://www.brucelindbloom.com/index.html?Eqn_ChromAdapt.html
  lazy val A: Illuminant = Illuminant(1.09850, 1.0, 0.35585)
  lazy val B: Illuminant = Illuminant(0.99072, 1.0, 0.85223)
  lazy val C: Illuminant = Illuminant(0.98074, 1.0, 1.18232)
  lazy val D50: Illuminant = Illuminant(0.96422, 1.0, 0.82521)
  lazy val D55: Illuminant = Illuminant(0.95682, 1.0, 0.92149)
  lazy val D65: Illuminant = Illuminant(0.95047, 1.0, 1.08883)
  lazy val D75: Illuminant = Illuminant(0.94972, 1.0, 1.22638)
  lazy val E: Illuminant = Illuminant(1.0, 1.0, 1.0)
  lazy val F2: Illuminant = Illuminant(0.99186, 1.0, 0.67393)
  lazy val F7: Illuminant = Illuminant(0.95041, 1.0, 1.08747)
  lazy val F11: Illuminant = Illuminant(1.00962, 1.0, 0.64350)
}

// https://en.wikipedia.org/wiki/Standard_illuminant
case class Illuminant(xₙ: Double, val yₙ: Double /* Always 1.0? */, zₙ: Double) {
  lazy val `1/xₙ`: Double = 1.0 / xₙ
  lazy val `1/zₙ`: Double = 1.0 / zₙ
  lazy val `1/yₙ`: Double = 1.0 / yₙ
  lazy val whitePointValues: ARRAY[Double] = ARRAY[Double](xₙ, yₙ, zₙ)
  lazy val asColumnMatrix:Matrix = Matrix(
    ARRAY[ARRAY[Double]](
      ARRAY[Double](xₙ),
      ARRAY[Double](yₙ),
      ARRAY[Double](zₙ)
    )
  )
}