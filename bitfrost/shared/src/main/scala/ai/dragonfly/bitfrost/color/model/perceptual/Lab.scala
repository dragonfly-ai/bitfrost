package ai.dragonfly.bitfrost.color.model.perceptual

import ai.dragonfly.bitfrost.ColorContext
import ai.dragonfly.bitfrost.cie.*
import ai.dragonfly.bitfrost.cie.Constant.*
import ai.dragonfly.math.stats.geometry.Tetrahedron
import ai.dragonfly.math.vector.{Vector3, VectorValues, dimensionCheck}
import ai.dragonfly.math.{Random, cubeInPlace}

trait Lab { self: WorkingSpace =>

  object Lab extends LStarSpace[Lab] {

    def apply(values: VectorValues): Lab = new Lab(dimensionCheck(values, 3))

    /**
     * @param L the L* component of the CIE L*a*b* color.
     * @param a the a* component of the CIE L*a*b* color.
     * @param b the b* component of the CIE L*a*b* color.
     * @return an instance of the LAB case class.
     * @example {{{ val c = LAB(72.872, -0.531, 71.770) }}}
     */
    def apply(L: Double, a: Double, b: Double): Lab = apply(VectorValues(L, a, b))

    inline def f(t: Double): Double = if (t > ϵ) Math.cbrt(t) else (t * `k/116`) + `16/116`

    /**
     * Requires a reference 'white' because although black provides a lower bound for XYZ values, they have no upper bound.
     *
     * @param xyz
     * @param illuminant
     * @return
     */
    def fromXYZ(xyz: XYZ): Lab = {
      val fy: Double = f(illuminant.`1/yₙ` * xyz.y)

      apply(
        116.0 * fy - 16.0,
        500.0 * (f(illuminant.`1/xₙ` * xyz.x) - fy),
        200.0 * (fy - f(illuminant.`1/zₙ` * xyz.z))
      )
    }

//    override val rgbGamut:Gamut = Gamut.fromRGB(transform = (v:XYZ) => Vector3(fromXYZ(v).values))
//    override def toString:String = s"${illuminant}L*a*b*"
  }

  case class Lab private(override val values: VectorValues) extends LStarModel[Lab] {
    override type VEC = this.type with Lab

    override def copy(): VEC = new Lab(VectorValues(L, a, b)).asInstanceOf[VEC]

    inline def L: Double = values(0)

    inline def a: Double = values(1)

    inline def b: Double = values(2)

    inline def fInverse(t: Double): Double = if (t > `∛ϵ`) cubeInPlace(t) else (`116/k` * t) - `16/k`

    def toXYZ: XYZ = {
      val white: XYZ = XYZ(illuminant.whitePointValues)
      val fy: Double = `1/116` * (L + 16.0)

      XYZ(
        fInverse((0.002 * a) + fy) * white.x, // X
        (if (L > kϵ) {
          val l = L + 16.0;
          `1/116³` * (l * l * l)
        } else `1/k` * L) * white.y, // Y
        fInverse(fy - (0.005 * b)) * white.z, // X
      )
    }

    override def similarity(that: Lab): Double = {
      Lab.similarity(this, that)
    }

    override def toRGB:RGB = toXYZ.toRGB

    override def toString: String = s"L*a*b*($L,$a,$b)"
  }

}
