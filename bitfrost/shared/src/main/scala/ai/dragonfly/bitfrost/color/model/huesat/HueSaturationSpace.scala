package ai.dragonfly.bitfrost.color.model.huesat

import ai.dragonfly.bitfrost.NormalizedValue
import ai.dragonfly.bitfrost.cie.WorkingSpace
import ai.dragonfly.bitfrost.color.model.*
import ai.dragonfly.math.squareInPlace
import ai.dragonfly.math.vector.*

trait HueSaturation { self: WorkingSpace =>

  trait HueSaturation[C <: HueSaturation[C]] extends CylindricalModel[C] {
    def hue: Double
    def saturation: Double

    override def toXYZ: XYZ = toRGB.toXYZ
  }

  trait HueSaturationSpace[C <: HueSaturation[C]] extends CylindricalSpace[C] with NormalizedValue {

    def apply(h: Double, s: Double, lv: Double): C

    inline def validHue(angle: Double): Boolean = angle >= 0f && angle <= 360.0

    inline def clampHue(angle: Double): Double = ((angle % 360.0d) + 360.0d) % 360.0d // Aly Cerruti's angle santization function from nose

    inline def hueMinMax(red: Double, green: Double, blue: Double): VectorValues = {
      // hue extractor based on a scala implementation in project nose: https://gitlab.com/srnb/nose/-/blob/master/nose/src/main/scala/tf/bug/nose/space/rgb/StandardRGB.scala
      // provided by Aly Cerruti

      val min: Double = Math.min(red, Math.min(green, blue))
      val MAX: Double = Math.max(red, Math.max(green, blue))

      VectorValues(
        clampHue(
          MAX match {
            case `min` => 0.0
            case `red` => 60.0 * ((green - blue) / (MAX - min))
            case `green` => 60.0 * (2.0d + ((blue - red) / (MAX - min)))
            case `blue` => 60.0 * (4.0d + ((red - green) / (MAX - min)))
          }
        ),
        min,
        MAX
      )
    }

    def asVector3(c: C): Vector3 = Vector3(
      c.values(1) * Math.cos(c.values(0)),
      c.values(1) * Math.sin(c.values(0)),
      c.values(2)
    )

    override val maxDistanceSquared: Double = 6.0

    override def similarity(c1: C, c2: C): Double = asVector3(c1).euclid.distanceSquaredTo(asVector3(c2)) / maxDistanceSquared

    override def weightedAverage(c1: C, w1: Double, c2: C, w2: Double): C = {
      val avg: Vector3 = (asVector3(c1) * w1) + (asVector3(c2) * w2)
      apply(
        Math.atan(avg.x / avg.y),
        Math.sqrt(squareInPlace(avg.x) + squareInPlace(avg.y)),
        avg.z
      )
    }

    inline def hcxmToRGBvalues(hue: Double, c: Double, x: Double, m: Double): VectorValues = {
      val X = x + m
      val C = c + m

      if (hue < 60.0) clamp0to1(C, X, m) // hue = 360 clamps to 0
      else if (hue < 120.0) clamp0to1(X, C, m)
      else if (hue < 180.0) clamp0to1(m, C, X)
      else if (hue < 240.0) clamp0to1(m, X, C)
      else if (hue < 300.0) clamp0to1(X, m, C)
      else clamp0to1(C, m, X)
    }

    inline def XfromHueC(H: Double, C: Double): Double = C * (1.0 - Math.abs(((H / 60.0) % 2.0) - 1.0))

    override def fromXYZ(xyz: XYZ): C = fromRGB(xyz.toRGB)
  }
}