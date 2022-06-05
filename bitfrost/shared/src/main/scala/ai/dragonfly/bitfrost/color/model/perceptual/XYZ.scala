package ai.dragonfly.bitfrost.color.model.perceptual

import ai.dragonfly.bitfrost.cie.WorkingSpace
import ai.dragonfly.bitfrost.color.spectral.DEFAULT
import ai.dragonfly.bitfrost.visualization.VolumeMesh
import ai.dragonfly.math.vector.{Vector3, VectorValues, dimensionCheck}
import ai.dragonfly.math.matrix.util.given_Dimensioned_Matrix
import ai.dragonfly.math.matrix.util.asColumnMatrix

import scala.language.implicitConversions

/**
 * From: https://en.wikipedia.org/wiki/CIE_1931_color_space
 * "The CIE XYZ color space encompasses all color sensations that are visible to a person with average eyesight.
 * That is why CIE XYZ (Tristimulus values) is a device-invariant representation of color."
 *
 * "In the CIE 1931 model, Y is the luminance, Z is quasi-equal to blue (of CIE RGB), and X is a mix of the three CIE RGB
 * curves chosen to be nonnegative."
 *
 * "... the Z value is solely made up of the S cone response, the Y value a mix of L and M responses, and X value a mix
 * of all three. This fact makes XYZ values analogous to, but different from, the LMS cone responses of the human eye."
 */

trait XYZ { self:WorkingSpace =>

  object XYZ extends PerceptualSpace[XYZ] {

    override lazy val visibleGamut:Gamut = Gamut.fromSpectralSamples(
      cmf,
      (v:Vector3) => Vector3(
        whitePoint.x * v.x,
        whitePoint.y * v.y,
        whitePoint.z * v.z
      )
    )

    def apply(values: VectorValues): XYZ = new XYZ(dimensionCheck(values, 3))

    /**
     * @param L the L* component of the CIE L*a*b* color.
     * @param a the a* component of the CIE L*a*b* color.
     * @param b the b* component of the CIE L*a*b* color.
     * @return an instance of the LAB case class.
     * @example {{{ val c = LAB(72.872, -0.531, 71.770) }}}
     */

    def apply(x: Double, y: Double, z: Double): XYZ = apply(VectorValues(x, y, z))

    override def fromXYZ(xyz: XYZ): XYZ = apply(xyz.x, xyz.y, xyz.z)

    override lazy val theoreticalGamut: VolumeMesh = visibleGamut.volumeMesh

  }


  case class XYZ private(override val values: VectorValues) extends PerceptualModel[XYZ] {
    override type VEC = this.type with XYZ

    override def copy(): VEC = new XYZ(VectorValues(x, y, z)).asInstanceOf[VEC]

    inline def x: Double = values(0)

    inline def y: Double = values(1)

    inline def z: Double = values(2)

    override def similarity(that: XYZ): Double = XYZ.similarity(this, that)

    override def toXYZ: XYZ = copy()

    override def toRGB:RGB = {
      val temp: VectorValues = (M_inverse * Vector3(values).asColumnMatrix).getRowPackedCopy()
      for (i <- temp.indices) temp(i) = transferFunction.encode(temp(i))
      RGB(temp)
    }

    override def toString: String = s"XYZ($x,$y,$z)"
  }


}
