package ai.dragonfly.bitfrost.cie

import ai.dragonfly.math.vector.Vector3


/**
 * From: https://en.wikipedia.org/wiki/Standard_illuminant
 * "A standard illuminant is a theoretical source of visible light with a spectral power distribution that is published.
 * Standard illuminants provide a basis for comparing images or colors recorded under different lighting."
 */

object Illuminant {
  // illuminant values from: http://www.brucelindbloom.com/index.html?Eqn_ChromAdapt.html
  lazy val A: Illuminant = Illuminant(1.09850, 0.35585)
  lazy val B: Illuminant = Illuminant(0.99072, 0.85223)
  lazy val C: Illuminant = Illuminant(0.98074, 1.18232)
  lazy val D50: Illuminant = Illuminant(0.96422, 0.82521)
  lazy val D55: Illuminant = Illuminant(0.95682, 0.92149)
  lazy val D65: Illuminant = Illuminant(0.95047, 1.08883)
  lazy val D75: Illuminant = Illuminant(0.94972, 1.22638)
  lazy val E: Illuminant = Illuminant(1.0, 1.0)
  lazy val F2: Illuminant = Illuminant(0.99186, 0.67393)
  lazy val F7: Illuminant = Illuminant(0.95041, 1.08747)
  lazy val F11: Illuminant = Illuminant(1.00962, 0.64350)
}

// https://en.wikipedia.org/wiki/Standard_illuminant
case class Illuminant(Xn: Double, Zn: Double) {
  lazy val `1/Xn`: Double = 1.0 / Xn
  lazy val `1/Zn`: Double = 1.0 / Zn
  lazy val Yn: Double = 1.0
  lazy val `1/Yn`: Double = 0.01
  lazy val vector: Vector3 = Vector3(Xn, Yn, Zn)
}