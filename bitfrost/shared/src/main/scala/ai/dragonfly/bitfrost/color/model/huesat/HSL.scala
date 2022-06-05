package ai.dragonfly.bitfrost.color.model.huesat

import ai.dragonfly.bitfrost.*
import ai.dragonfly.bitfrost.cie.WorkingSpace
import ai.dragonfly.bitfrost.color.model.*
import ai.dragonfly.bitfrost.visualization.VolumeMesh
import ai.dragonfly.math.Random
import ai.dragonfly.math.vector.{VectorValues, dimensionCheck}

trait HSL extends HueSaturation { self: WorkingSpace =>
  object HSL extends HueSaturationSpace[HSL] {

    def apply(values: VectorValues): HSL = new HSL(dimensionCheck(values, 3))

    def clamp(values: VectorValues): HSL = {
      dimensionCheck(values, 3)
      clamp(values(0), values(1), values(2))
    }

    /**
     * HSL is the primary case class for representing colors in HSL space.
     *
     * @constructor Create a new HSV object from three Double values.  This constructor does not validate input parameters.
     *              For values taken from user input, sensors, or otherwise uncertain sources, consider using the factory method in the Color companion object.
     * @see [[ai.dragonfly.color.ColorVector.hsl]] for a method of constructing HSL objects that validates inputs.
     * @see [[https://en.wikipedia.org/wiki/HSL_and_HSV]] for more information about the HSL color space.
     * @param hue        an angle ranging from [0-360] degrees.  Values outside of this range may cause errors.
     * @param saturation a percentage ranging from [0-100].  Values outside of this range may cause errors.
     * @param lightness  a percentage ranging from [0-100].  Values outside of this range may cause errors.
     * @return an instance of the HSL case class.
     * @example {{{
     * val c = HSL(211f, 75f, 33.3333f)
     * c.toString()  // returns "HSL(211.000,75.000,33.333)"
     * }}}
     */
    def apply(hue: Double, saturation: Double, lightness: Double): HSL = new HSL(
      VectorValues(hue, saturation, lightness)
    )

    def clamp(hue: Double, saturation: Double, lightness: Double): HSL = new HSL(
      VectorValues(
        clampHue(hue),
        clamp0to1(saturation),
        clamp0to1(lightness)
      )
    )

    def fromRGB(nrgb: RGB): HSL = apply(toHSL(nrgb.red, nrgb.green, nrgb.blue))

    /**
     * Factory method for creating instances of the HSL class.  This method validates input parameters and throws an exception
     * if one or more of them lie outside of their allowed ranges.
     *
     * @param saturation an angle ranging from [0-360] degrees.
     * @param hue        a percentage ranging from [0-100].
     * @param lightness  a percentage ranging from [0-100].
     * @return an instance of the HSL case class.
     */
    def getIfValid(hue: Double, saturation: Double, lightness: Double): Option[HSL] = {
      if (validHue(hue) && valid0to1(saturation) && valid0to1(lightness)) Some(apply(hue, saturation, lightness))
      else None
    }

    inline def toHSL(red: Double, green: Double, blue: Double): VectorValues = {
      val values: VectorValues = hueMinMax(red, green, blue)

      val delta: Double = values(2 /*MAX*/) - values(1 /*min*/)
      val L: Double = (values(1 /*min*/) + values(2 /*MAX*/))

      values(1) = if (delta == 0.0) 0.0 else delta / (1.0 - Math.abs((L) - 1.0))
      values(2) = 0.5 * L // (min + max) / 2
      values
    }

    override def random(r: scala.util.Random = Random.defaultRandom): HSL = apply(
      VectorValues(
        r.nextDouble() * 360.0,
        r.nextDouble(),
        r.nextDouble()
      )
    )

    override def gamut: VolumeMesh = VolumeMesh.cylinder(sideSegments = 64)

  }

  case class HSL private(override val values: VectorValues) extends HueSaturation[HSL] {

    inline def hue: Double = values(0)

    inline def saturation: Double = values(1)

    inline def lightness: Double = values(2)

    override def similarity(that: HSL): Double = HSL.similarity(this, that)

    def copy(): HSL = new HSL(VectorValues(hue, saturation, lightness))

    def toRGB: RGB = {
      // https://www.rapidtables.com/convert/color/hsl-to-rgb.html
      val C = (1.0 - Math.abs((2 * lightness) - 1.0)) * saturation
      RGB.apply(
        HSL.hcxmToRGBvalues(
          hue,
          C,
          HSL.XfromHueC(hue, C), // X
          lightness - (0.5 * C) // m
        )
      )
    }

    override val toString: String = s"HSL($hue, $saturation, $lightness)"

    /**
     * @return a string representing the color in an SVG friendly way.
     * @example {{{
     * val c = HSL(211f, 75f, 33.3333f)
     * c.svg() // returns "hsl(211.000,75.0%,33.3%)"
     * }}}
     */
    def svg(): String = s"hsl(${f"$hue%1.3f"}, ${f"$saturation%1.1f"}%, ${f"$lightness%1.1f"}%)"
  }

}
