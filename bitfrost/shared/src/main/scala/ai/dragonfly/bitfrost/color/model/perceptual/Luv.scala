package ai.dragonfly.bitfrost.color.model.perceptual

import narr.*
import ai.dragonfly.bitfrost.ColorContext
import ai.dragonfly.bitfrost.cie.*
import ai.dragonfly.bitfrost.cie.Constant.*
import ai.dragonfly.math.stats.geometry.Tetrahedron
import ai.dragonfly.math.vector.*
import ai.dragonfly.math.{Random, cubeInPlace}

trait Luv { self: WorkingSpace =>

  object UV {

    private inline def getWeight(xyz: XYZ): Double = 1.0 / (xyz.x + xyz.y + xyz.z)

    def fromXYZ(xyz: XYZ): UV = {

      var w: Double = getWeight(xyz)

      val x: Double = w * xyz.x
      val y: Double = w * xyz.y
      w = 1.0 / (6.0 * y - x + 1.5)
      if (w.isNaN) UV(0.0, 0.0)
      UV(2.0 * x * w, 4.5 * y * w)

    }

  }

  case class UV(u: Double, v: Double) {
    def xy: Vector2 = {
      val denominator: Double = (6.0 * u) - (16.0 * v) + 12
      Vector2(
        (9.0 * u) / denominator, // X
        (9.0 * v) / denominator // y
      )
    }
  }

  object Luv extends PerceptualSpace[Luv] {

    def apply(values: NArray[Double]): Luv = new Luv(dimensionCheck(values, 3))

    /**
     * @constructor Create a new SlowSlimLuv object from three float values.  This constructor does not validate input parameters.
     * @param L the L* component of the CIE L*u*v* color.
     * @param u the u* component of the CIE L*u*v* color.
     * @param v the v* component of the CIE L*u*v* color.
     * @return an instance of the SlowSlimLuv case class.
     * @example {{{ val c = SlowSlimLuv(14.756, -3.756, -58.528) }}}
     */

    def apply(L: Double, u: Double, v: Double): Luv = apply(NArray[Double](u, v, L))

    // XYZ to LUV and helpers:

    val UV(uₙ: Double, vₙ: Double) = UV.fromXYZ(XYZ(illuminant.whitePointValues))

    inline def fL(t: Double): Double = if (t > ϵ) 116.0 * Math.cbrt(t) - 16.0 else k * t

    def fromXYZ(xyz: XYZ): Luv = {

      val `Y/Yₙ`: Double = xyz.y / illuminant.yₙ

      val `L⭑` = fL(`Y/Yₙ`)

      val uv: UV = UV.fromXYZ(xyz)

      val `u⭑`:Double = 13.0 * `L⭑` * (uv.u - uₙ)
      val `v⭑`:Double = 13.0 * `L⭑` * (uv.v - vₙ)

      apply(
        `L⭑`,
        if (`u⭑`.isNaN) 0.0 else `u⭑`,
        if (`v⭑`.isNaN) 0.0 else `v⭑`
      )
    }

//    override val rgbGamut:Gamut = Gamut.fromRGB(transform = (v:XYZ) => Vector3(fromXYZ(v).values))

    //    override def toString:String = s"${illuminant}L*u*v*"
  }

  /**
   * LUV is the base trait for classes that encode colors in the CIE L*u*v* color space.
   *
   * @see [[https://en.wikipedia.org/wiki/CIELUV]] for more information on CIE L*u*v*.
   */

  case class Luv private(override val values: NArray[Double]) extends PerceptualModel[Luv] {
    override type VEC = this.type with Luv

    inline def L: Double = values(2)

    inline def u: Double = values(0)

    inline def v: Double = values(1)

    override def toString: String = s"L⭑u⭑v⭑($L,$u,$v)"

    override def copy(): VEC = new Luv(NArray[Double](u, v, L)).asInstanceOf[VEC]

    // LUV to XYZ and helpers:
    inline def flInverse(t: Double): Double = if (t > kϵ) {
      cubeInPlace(`1/116` * (t + 16.0)) //`1/116³` * cubeInPlace(t + 16.0) // ((L+16)/116)^3 = (L + 16)^3 / 116^3 = (L + 16)^3 / 1560896.0
    } else `1/k` * t

    def toXYZ: XYZ = {

      val uₓ: Double = (u / (13.0 * L)) + Luv.uₙ
      val vₓ: Double = (v / (13.0 * L)) + Luv.vₙ

      val Y: Double = flInverse(L)
      val X: Double = 9.0 * Y * uₓ / (4.0 * vₓ)
      val Z: Double = (3.0 * Y / vₓ) - (5.0 * Y) - (X / 3.0)

      XYZ(X, Y, Z)
    }

    override def toRGB: RGB = toXYZ.toRGB

    override def similarity(that: Luv): Double = {
      Luv.similarity(this, that)
    }

  }

}
