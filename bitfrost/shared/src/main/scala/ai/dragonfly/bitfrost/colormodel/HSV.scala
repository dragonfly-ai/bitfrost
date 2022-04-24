package ai.dragonfly.bitfrost.colormodel

import ai.dragonfly.bitfrost.{Hue, Saturation, Value}
import ai.dragonfly.bitfrost.colorspace.WorkingSpace
import ai.dragonfly.math.Random
import ai.dragonfly.math.vector.{VectorValues, dimensionCheck}

trait HSV { self: WorkingSpace =>

  object HSV extends CommonColorSpace[HSV] {

    def apply(values: VectorValues): HSV = new HSV(dimensionCheck(values, 3))


    def clamp(values: VectorValues): HSV = {
      dimensionCheck(values, 3)
      clamp( values(0), values(1), values(2) )
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

    def apply(hue: Double, saturation: Double, value: Double): HSV = new HSV(VectorValues(hue, saturation, value))


    def clamp(hue: Double, saturation: Double, value: Double): HSV = new HSV(
      VectorValues(
        hue, //Hue.clamp(hue),
        Saturation.clamp(saturation),
        Value.clamp(value)
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
      if (Hue.valid(hue) && Saturation.valid(saturation) && Value.valid(saturation)) Some(apply(hue, saturation, value))
      else None
    }

    override def fromNRGB(nrgb: NRGB): HSV = apply(Hue.toHSV(nrgb.red, nrgb.green, nrgb.blue))

    override def random(r: scala.util.Random = Random.defaultRandom): HSV = apply(
      VectorValues(
        r.nextDouble() * 360.0,
        r.nextDouble(),
        r.nextDouble()
      )
    )

  }

  case class HSV private(override val values: VectorValues) extends CommonColor[HSV] {
    override type VEC = this.type with HSV

    inline def hue: Double = values(0)

    inline def saturation: Double = values(1)

    inline def value: Double = values(2)

    // https://www.rapidtables.com/convert/color/hsv-to-rgb.html
    override def toNRGB: NRGB = {
      val C = value * saturation
      NRGB.apply(Hue.hcxmToNRGBvalues(hue, C, Hue.XfromHueC(hue, C), value - C))
    }

    override def copy(): VEC = new HSV(VectorValues(hue, saturation, value)).asInstanceOf[VEC]

    override val toString: String = s"HSV($hue, $saturation, $value)"
  }

}
