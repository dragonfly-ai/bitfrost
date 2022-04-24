package ai.dragonfly.bitfrost.colormodel

import Jama.Matrix
import ai.dragonfly.bitfrost.{RGB, XYZ}
import ai.dragonfly.bitfrost.colorspace.WorkingSpace
import ai.dragonfly.math.{Random, matrix}
import ai.dragonfly.math.matrix.MatrixValues
import ai.dragonfly.math.matrix.util.given_Dimensioned_Matrix
import ai.dragonfly.math.matrix.util.asColumnMatrix
import ai.dragonfly.math.vector.{Vector3, VectorValues, dimensionCheck}

trait NRGB { self: WorkingSpace =>

  object NRGB extends CommonColorSpace[NRGB] {
    val `1/255`: Double = 1.0 / 255.0

    def apply(values: VectorValues): NRGB = new NRGB(dimensionCheck(values, 3))

    /**
     * Factory method to create a fully opaque NARGB instance from separate, specified red, green, blue components and
     * a default alpha value of 1.0.
     * Double values that range outside of [0.0-1.0] may give unexpected results.  For values taken from user input, sensors,
     * or otherwise uncertain sources, consider using the factory method in the Color companion object.
     *
     * @see [[ai.dragonfly.color.ColorVectorSpace.NARGB]] for a method of constructing NARGB objects that validates inputs.
     * @param red   decimal value from [0.0-1.0] representing the red component in RGB space.
     * @param green decimal value from [0.0-1.0] representing the green component in RGB space.
     * @param blue  decimal value from [0.0-1.0] representing the blue component in RGB space.
     * @return an instance of the NARGB case class.
     * @example {{{ val c = NARGB(72,105,183) }}}
     */
    def apply(red: Double, green: Double, blue: Double): NRGB = apply(VectorValues(red, green, blue))

    //val `1/255`: Double = 0.00392156862745098

    /**
     * Factory method to create a NARGB instance from an ARGB instance.
     *
     * @param argb instance of ARGB.
     * @return an instance of the NARGB case class.
     * @example {{{ val c = NARGB(72,105,183) }}}
     */
    def apply(argb: ARGB): NRGB = apply(`1/255` * argb.red, `1/255` * argb.green, `1/255` * argb.blue)

    /**
     * Factory method to create a fully Opaque NARGB color; one with an alpha value of 1.0.
     * Because this method validates each intensity, it sacrifices performance for safety.
     * Although well suited for parsing color data generated by sensors or user input, this method undercuts
     * performance in applications like reading image data.
     *
     * To skip validation and minimize overhead, @see [[ai.dragonfly.color.NRGB.apply]]
     *
     * @param red   decimal value from [0.0-1.0] representing the red component in RGB space.
     * @param green decimal value from [0.0-1.0] representing the green component in RGB space.
     * @param blue  decimal value from [0.0-1.0] representing the blue component in RGB space.
     * @return an instance of the NARGB class or None if fed invalid input.
     */

    def getIfValid(red: Double, green: Double, blue: Double): Option[NRGB] = {
      if (valid(red, green, blue)) Some(apply(red, green, blue))
      else None
    }

    inline def valid(i: Double): Boolean = 0.0 <= i && i <= 1.0

    inline def valid(red: Double, green: Double, blue: Double): Boolean = valid(red) && valid(green) && valid(blue)

    /**
     * Use Color.random() to obtain a random color in the form of an NARGB instance.
     * This method executes quickly and without memory costs, but the RGB color space biases toward cool colors.
     * In contrast, the Color.randomFromLabSpace() method takes seconds to initialize and has a memory footprint of several megabytes
     * However, it samples from a perceptually uniform color space and avoids the bias toward cool colors.
     * This method samples the Red, Green, and Blue color components uniformly, but always returns 1.0 for the alpha component.
     *
     * @return a randomly generated color sampled from the RGB Color Space.
     */
    override def random(r: scala.util.Random = Random.defaultRandom): NRGB = apply(
      VectorValues(r.nextDouble(), r.nextDouble(), r.nextDouble())
    )

    override def fromNRGB(nrgb: NRGB): NRGB = nrgb.copy()
  }

  case class NRGB private(override val values: VectorValues) extends CommonColor[NRGB] {
    override type VEC = this.type with NRGB

    inline def red: Double = values(0)

    inline def green: Double = values(1)

    inline def blue: Double = values(2)

    override val toString: String = s"NRGB($red, $green, $blue)"

    override def copy(): VEC = new NRGB(VectorValues(red, green, blue)).asInstanceOf[VEC]

    def toXYZ: XYZ = Vector3(
      (M * new Matrix(
        MatrixValues(
          VectorValues(compander.decode(red)),
          VectorValues(compander.decode(green)),
          VectorValues(compander.decode(blue))
        )
      )).getRowPackedCopy()
    )

    def toARGB: ARGB = ARGB(
      RGB.clamp( 255.0, red * 255.0, green * 255.0, blue * 255.0 )
    )

    inline def toNRGB: NRGB = this
  }

}