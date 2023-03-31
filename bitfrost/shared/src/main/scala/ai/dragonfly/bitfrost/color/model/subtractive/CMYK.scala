package ai.dragonfly.bitfrost.color.model.subtractive

import narr.*
import ai.dragonfly.bitfrost.cie.WorkingSpace
import ai.dragonfly.mesh.*
import ai.dragonfly.mesh.shape.*
import ai.dragonfly.bitfrost.{ColorContext, NormalizedValue}
import ai.dragonfly.math.Random
import ai.dragonfly.math.vector.*

trait CMYK { self: WorkingSpace =>

  object CMYK extends VectorSpace[CMYK] with NormalizedValue {

    override val maxDistanceSquared: Double = 4.0

    def apply(values: NArray[Double]): CMYK = {
      if (values.length == 3) apply(values(0), values(1), values(2))
      else new CMYK(dimensionCheck(values, 4))
    }

    def apply(cyan: Double, magenta: Double, yellow: Double): CMYK = {
      val values: NArray[Double] = NArray[Double](
        cyan,
        magenta,
        yellow,
        0
      )

      values(3) = Math.min(values(0), Math.min(values(1), values(2)))

      values(0) = values(0) - values(3)
      values(1) = values(1) - values(3)
      values(2) = values(2) - values(3)

      apply(values)
    }

    def apply(cyan: Double, magenta: Double, yellow: Double, key: Double): CMYK = apply(NArray[Double](cyan, magenta, yellow, key))


    /**
     * Factory method for creating instances of the CMYK class.
     * This method validates input parameters at the cost of some performance.
     *
     * @param cyan    a value between [0-1]
     * @param magenta a value between [0-1]
     * @param yellow  a value between [0-1]
     * @param key   a value between [0-1]
     * @return an instance of the CMYK class.
     */
    def getIfValid(cyan: Double, magenta: Double, yellow: Double, key: Double): Option[CMYK] = {
      if (valid0to1(key) && valid0to1(cyan, magenta, yellow) && valid0to1(cyan + key, magenta + key, yellow + key)) {
        Some(apply(cyan, magenta, yellow, key))
      }
      else None
    }

    override def random(r: scala.util.Random = Random.defaultRandom): CMYK = apply(
      r.nextDouble(),
      r.nextDouble(),
      r.nextDouble()
    )

    override def fromXYZ(xyz: XYZ): CMYK = fromRGB(xyz.toRGB)

    def fromRGB(rgb: RGB): CMYK = {
      // http://color.lukas-stratmann.com/color-systems/cmy.html
      val k:Double = 1.0 - Math.max(rgb.red, Math.max(rgb.green, rgb.blue))
      apply(
        clamp0to1(
          (1.0 - rgb.red) - k,
          (1.0 - rgb.green) - k,
          (1.0 - rgb.blue) - k,
          k
        )
      )
    }

    override def toVector3(c: CMYK): Vector3 = Vector3(
      c.cyan + c.key,
      c.yellow + c.key,
      c.magenta + c.key
    )

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
   * @param key   a value ranging from [0-1].  Values outside of this range may cause errors.
   * @return an instance of the CMYK case class.
   * @example {{{
   * val c = CMYK(1f, 0.25f, 0.5f, 0f)
   * c.toString()  // returns "CMYK(1.000,0.250,0.500,0.000)"
   * }}}
   */

  case class CMYK private(override val values: NArray[Double]) extends VectorModel[CMYK] {
    override type VEC = this.type with CMYK

    inline def cyan: Double = values(0)

    inline def magenta: Double = values(1)

    inline def yellow: Double = values(2)

    inline def key: Double = values(3)

    inline def black: Double = values(3)

    override def toXYZ: XYZ = toRGB.toXYZ

    override def toRGB: RGB = {
      // http://color.lukas-stratmann.com/color-systems/cmy.html
      RGB.apply(
        RGB.clamp0to1(
          1.0 - (cyan + key),
          1.0 - (magenta + key),
          1.0 - (yellow + key)
        )
      )

      // https://www.rapidtables.com/convert/color/cmyk-to-rgb.html
//      RGB.apply(
//        RGB.clamp0to1(
//          (1.0 - cyan) * (1.0 - key),
//          (1.0 - magenta) * (1.0 - key),
//          (1.0 - yellow) * (1.0 - key)
//        )
//      )
    }
    override def similarity(that: CMYK): Double = CMYK.similarity(this, that)

    override def toString: String = s"CMYK($cyan, $magenta, $yellow, $key)"

    override def copy(): VEC = new CMYK(NArray[Double](cyan, magenta, yellow, key)).asInstanceOf[VEC]
  }

}
