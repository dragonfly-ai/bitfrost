package ai.dragonfly.bitfrost.color.model.huesat

import narr.*
import ai.dragonfly.bitfrost.*
import ai.dragonfly.bitfrost.cie.WorkingSpace
import ai.dragonfly.bitfrost.color.model.*
import ai.dragonfly.mesh.*
import ai.dragonfly.mesh.shape.*
import ai.dragonfly.math.Random
import ai.dragonfly.math.vector.*

trait HSV extends HueSaturation { self: WorkingSpace =>

  object HSV extends HueSaturationSpace[HSV] {

    def apply(values: NArray[Double]): HSV = new HSV(dimensionCheck(values, 3))

    def clamp(values: NArray[Double]): HSV = {
      dimensionCheck(values, 3)
      clamp(values(0), values(1), values(2))
    }

    /**
     * HSV is the primary case class for representing colors in HSV space.
     *
     * @constructor Create a new HSV object from three Double values.  This constructor does not validate
     *              input parameters.  For values taken from user input, sensors, or otherwise uncertain sources, consider using
     *              the factory method in the Color companion object.
     * @see [[ai.dragonfly.color.HSV.getIfValid]] for a method of constructing HSV objects that validates inputs.
     * @see [[https://en.wikipedia.org/wiki/HSL_and_HSV]] for more information about the HSV color space.
     * @param hue        an angle ranging from [0-360] degrees.  Values outside of this range may cause errors.
     * @param saturation a percentage ranging from [0-100].  Values outside of this range may cause errors.
     * @param value      a percentage ranging from [0-100].  Values outside of this range may cause errors.
     * @return an instance of the HSV case class.
     * @example {{{
     * val c = HSV(211f, 75f, 33.3333f)
     * c.toString()  // returns "HSV(211.000,75.000,33.333)"
     * }}}
     */

    def apply(hue: Double, saturation: Double, value: Double): HSV = new HSV(NArray[Double](hue, saturation, value))


    def clamp(hue: Double, saturation: Double, value: Double): HSV = new HSV(
      NArray[Double](
        clampHue(hue),
        clamp0to1(saturation),
        clamp0to1(value)
      )
    )

    /**
     * Factory method for creating instances of the HSV class.  This method validates input parameters and throws an exception
     * if one or more of them lie outside of their allowed ranges.
     *
     * @param saturation an angle ranging from [0-360] degrees.
     * @param hue        a percentage ranging from [0-100].
     * @param value      a percentage ranging from [0-100].
     * @return an instance of the HSV case class.
     */
    def getIfValid(hue: Double, saturation: Double, value: Double): Option[HSV] = {
      if (validHue(hue) && valid0to1(saturation) && valid0to1(saturation)) Some(apply(hue, saturation, value))
      else None
    }

    def fromRGB(nrgb: RGB): HSV = apply(toHSV(nrgb.red, nrgb.green, nrgb.blue))


    inline def toHSV(red: Double, green: Double, blue: Double): NArray[Double] = {
      val values: NArray[Double] = hueMinMax(red, green, blue)
      values(1) = {  // S
        if (values(2 /*MAX*/) == 0.0) 0.0
        else (values(2 /*MAX*/) - values(1 /*min*/)) / values(2 /*MAX*/)
      }
      values
    }

    override def random(r: scala.util.Random = Random.defaultRandom): HSV = apply(
      NArray[Double](
        r.nextDouble() * 360.0,
        r.nextDouble(),
        r.nextDouble()
      )
    )

  }

  case class HSV private(override val values: NArray[Double]) extends HueSaturation[HSV] {

    inline def hue: Double = values(0)

    inline def saturation: Double = values(1)

    inline def value: Double = values(2)

    // https://www.rapidtables.com/convert/color/hsv-to-rgb.html
    def toRGB: RGB = {
      val C = value * saturation
      RGB.apply(HSV.hcxmToRGBvalues(hue, C, HSV.XfromHueC(hue, C), value - C))
    }

    def copy(): HSV = new HSV(NArray[Double](hue, saturation, value))

    override def similarity(that: HSV): Double = HSV.similarity(this, that)

    override val toString: String = s"HSV($hue, $saturation, $value)"
  }

}
