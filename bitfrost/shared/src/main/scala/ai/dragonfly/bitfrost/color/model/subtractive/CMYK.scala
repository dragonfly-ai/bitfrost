package ai.dragonfly.bitfrost.color.model.subtractive

import ai.dragonfly.bitfrost.cie.WorkingSpace

import ai.dragonfly.bitfrost.{ColorContext, NormalizedValue}
import ai.dragonfly.math.Random
import ai.dragonfly.math.vector.{VectorValues, dimensionCheck}

trait CMYK { self: WorkingSpace =>

  object CMYK extends VectorSpace[CMYK] with NormalizedValue {

    override val maxDistanceSquared: Double = 4.0

    def apply(values: VectorValues): CMYK = new CMYK(dimensionCheck(values, 4))

    def apply(cyan: Double, magenta: Double, yellow: Double, black: Double): CMYK = apply(VectorValues(cyan, magenta, yellow, black))


    /**
     * Factory method for creating instances of the CMYK class.
     * This method validates input parameters at the cost of some performance.
     *
     * @param cyan    a value between [0-1]
     * @param magenta a value between [0-1]
     * @param yellow  a value between [0-1]
     * @param black   a value between [0-1]
     * @return an instance of the CMYK class.
     */
    def getIfValid(cyan: Double, magenta: Double, yellow: Double, black: Double): Option[CMYK] = {
      if (valid0to1(cyan, magenta, yellow, black)) Some(apply(cyan, magenta, yellow, black))
      else None
    }

    override def random(r: scala.util.Random = Random.defaultRandom): CMYK = apply(
      VectorValues(
        r.nextDouble(),
        r.nextDouble(),
        r.nextDouble(),
        r.nextDouble()
      )
    )

    override def fromXYZ(xyz: XYZ): CMYK = fromRGB(xyz.toRGB)

    def fromRGB(rgb: RGB): CMYK = {
      val K = 1.0 - Math.max(rgb.red, Math.max(rgb.green, rgb.blue))
      val kInv = 1.0 - K
      val C = (1.0 - rgb.red - K) / kInv
      val M = (1.0 - rgb.green - K) / kInv
      val Y = (1.0 - rgb.blue - K) / kInv

      CMYK(C, M, Y, K)
    }

  }

  /**
   * CMYK is the primary case class for representing colors in CMYK space.
   *
   * @constructor Create a new CMYK object from three Double values.  This constructor does not validate input parameters.
   *              For values taken from user input, sensors, or otherwise uncertain sources, consider using the factory method in the Color companion object.
   * @see [[ai.dragonfly.color.CMYK.getIfValid]] for a method of constructing CMYK objects that validates inputs.
   * @see [[https://en.wikipedia.org/wiki/CMYK_color_model]] for more information about the CMYK color space.
   * @param cyan    a value ranging from [0-1].  Values outside of this range may cause errors.
   * @param magenta a value ranging from [0-1].  Values outside of this range may cause errors.
   * @param yellow  a value ranging from [0-1].  Values outside of this range may cause errors.
   * @param black   a value ranging from [0-1].  Values outside of this range may cause errors.
   * @return an instance of the CMYK case class.
   * @example {{{
   * val c = CMYK(1f, 0.25f, 0.5f, 0f)
   * c.toString()  // returns "CMYK(1.000,0.250,0.500,0.000)"
   * }}}
   */

  case class CMYK private(override val values: VectorValues) extends VectorModel[CMYK] {
    override type VEC = this.type with CMYK

    inline def cyan: Double = values(0)

    inline def magenta: Double = values(1)

    inline def yellow: Double = values(2)

    inline def black: Double = values(3)

    override def toXYZ: XYZ = toRGB.toXYZ

    override def toRGB: RGB = RGB.apply(
      RGB.clamp0to1(
        (1.0 - cyan) * (1.0 - black),
        (1.0 - magenta) * (1.0 - black),
        (1.0 - yellow) * (1.0 - black)
      )
    )

    override def similarity(that: CMYK): Double = CMYK.similarity(this, that)

    override def toString: String = s"CMYK($cyan, $magenta, $yellow, $black)"

    override def copy(): VEC = new CMYK(VectorValues(cyan, magenta, yellow, black)).asInstanceOf[VEC]
  }

}
