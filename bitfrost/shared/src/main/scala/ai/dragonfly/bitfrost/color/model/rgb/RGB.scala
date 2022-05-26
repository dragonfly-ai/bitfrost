package ai.dragonfly.bitfrost.color.model.rgb

import Jama.Matrix
import ai.dragonfly.bitfrost.*
import ai.dragonfly.bitfrost.cie.WorkingSpace
import ai.dragonfly.bitfrost.color.model.*
import ai.dragonfly.bitfrost.NormalizedValue
import ai.dragonfly.math.Random
import ai.dragonfly.math.matrix.MatrixValues
import ai.dragonfly.math.vector.{Vector3, VectorValues, dimensionCheck}
import ai.dragonfly.math.matrix.util.given_Dimensioned_Matrix
import ai.dragonfly.math.matrix.util.asColumnMatrix

import scala.language.implicitConversions

trait RGB { self: WorkingSpace =>

  val `1/255`: Double = 1.0 / 255.0

  object RGB extends VectorSpace[RGB] with NormalizedValue {

    override val maxDistanceSquared: Double = 9.0

    def apply(values: VectorValues): RGB = new RGB(dimensionCheck(values, 3))

    /**
     * Factory method to create a fully opaque RGB instance from separate, specified red, green, blue components and
     * a default alpha value of 1.0.
     * Double values that range outside of [0.0-1.0] may give unexpected results.  For values taken from user input, sensors,
     * or otherwise uncertain sources, consider using the factory method in the Color companion object.
     *
     * @see [[ai.dragonfly.color.ColorVectorSpace.RGB]] for a method of constructing RGB objects that validates inputs.
     * @param red   decimal value from [0.0-1.0] representing the red component in RGB space.
     * @param green decimal value from [0.0-1.0] representing the green component in RGB space.
     * @param blue  decimal value from [0.0-1.0] representing the blue component in RGB space.
     * @return an instance of the RGB case class.
     * @example {{{ val c = RGB(72,105,183) }}}
     */
    def apply(red: Double, green: Double, blue: Double): RGB = apply(VectorValues(red, green, blue))

//    def apply(argb: ARGB32): RGB = apply(`1/255` * argb.red, `1/255` * argb.green, `1/255` * argb.blue)
//
//    def apply(rgba: RGBA32): RGB = apply(`1/255` * rgba.red, `1/255` * rgba.green, `1/255` * rgba.blue)

    override def fromXYZ(xyz:XYZ):RGB = {
      val temp: VectorValues = (M_inverse * Vector3(xyz.values).asColumnMatrix).getRowPackedCopy()
      for (i <- temp.indices) temp(i) = transferFunction.encode(temp(i))
      apply(temp)
    }

    override def fromRGB(rgb: RGB): RGB = apply(rgb.red, rgb.green, rgb.blue)

    /**
     * Factory method to create a fully Opaque RGB color; one with an alpha value of 1.0.
     * Because this method validates each intensity, it sacrifices performance for safety.
     * Although well suited for parsing color data generated by sensors or user input, this method undercuts
     * performance in applications like reading image data.
     *
     * To skip validation and minimize overhead, @see [[ai.dragonfly.color.RGB.apply]]
     *
     * @param red   decimal value from [0.0-1.0] representing the red component in RGB space.
     * @param green decimal value from [0.0-1.0] representing the green component in RGB space.
     * @param blue  decimal value from [0.0-1.0] representing the blue component in RGB space.
     * @return an instance of the RGB class or None if fed invalid input.
     */

    def getIfValid(red: Double, green: Double, blue: Double): Option[RGB] = {
      if (valid0to1(red, green, blue)) Some(apply(red, green, blue))
      else None
    }

    /**
     * Use Color.random() to obtain a random color in the form of an RGB instance.
     * This method executes quickly and without memory costs, but the RGB color space biases toward cool colors.
     * In contrast, the Color.randomFromLabSpace() method takes seconds to initialize and has a memory footprint of several megabytes
     * However, it samples from a perceptually uniform color space and avoids the bias toward cool colors.
     * This method samples the Red, Green, and Blue color components uniformly, but always returns 1.0 for the alpha component.
     *
     * @return a randomly generated color sampled from the RGB Color Space.
     */
    override def random(r: scala.util.Random = Random.defaultRandom): RGB = apply(
      VectorValues(r.nextDouble(), r.nextDouble(), r.nextDouble())
    )

  }

  case class RGB private(override val values: VectorValues) extends VectorModel[RGB] {
    override type VEC = this.type with RGB

    inline def red: Double = values(0)

    inline def green: Double = values(1)

    inline def blue: Double = values(2)

    override val toString: String = s"RGB($red, $green, $blue)"

    override def copy(): VEC = new RGB(VectorValues(red, green, blue)).asInstanceOf[VEC]

    def toXYZ: XYZ = XYZ(
      (M * new Matrix(
        MatrixValues(
          VectorValues(transferFunction.decode(red)),
          VectorValues(transferFunction.decode(green)),
          VectorValues(transferFunction.decode(blue))
        )
      )).getRowPackedCopy().asInstanceOf[VectorValues]
    )

    override def similarity(that: RGB): Double = RGB.similarity(this, that)

    override def toRGB: RGB = RGB(red, green, blue)
  }

}
