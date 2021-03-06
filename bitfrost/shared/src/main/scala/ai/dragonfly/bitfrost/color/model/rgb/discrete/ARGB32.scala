package ai.dragonfly.bitfrost.color.model.rgb.discrete

import ai.dragonfly.bitfrost.*
import ai.dragonfly.bitfrost.cie.WorkingSpace
import ai.dragonfly.bitfrost.color.model.*
import ai.dragonfly.math.{Random, squareInPlace}

import scala.language.implicitConversions

trait ARGB32 extends DiscreteRGB { self: WorkingSpace =>

  given Conversion[java.awt.Color, ARGB32] with
    def apply(jac: java.awt.Color): ARGB32 = ARGB32(jac.getRGB())

  given Conversion[ARGB32, java.awt.Color] with
    def apply(c: ARGB32): java.awt.Color = new java.awt.Color(c.argb, true)

  given Conversion[Int, ARGB32] with
    def apply(argb: Int): ARGB32 = ARGB32(argb)

  given Conversion[ARGB32, Int] with
    def apply(c: ARGB32): Int = c.argb

  object ARGB32 extends UtilRGB32[ARGB32] {

    def apply(argb: Int): ARGB32 = new ARGB32(argb)

    /**
     * Factory method to create a fully opaque ARGB instance from separate, specified red, green, blue components and
     * a default alpha value of 255.
     * Parameter values are derived from the least significant byte.  Integer values that range outside of [0-255] may
     * give unexpected results.  For values taken from user input, sensors, or otherwise uncertain sources, consider using
     * the factory method in the Color companion object.
     *
     * @see [[ai.dragonfly.color.ColorVectorSpace.argb]] for a method of constructing ARGB objects that validates inputs.
     * @param red   integer value from [0-255] representing the red component in RGB space.
     * @param green integer value from [0-255] representing the green component in RGB space.
     * @param blue  integer value from [0-255] representing the blue component in RGB space.
     * @return an instance of the ARGB case class.
     * @example {{{ val c = ARGB32(72,105,183) }}}
     */
    def apply(red: Int, green: Int, blue: Int): ARGB32 = apply(255, red, green, blue)


    /**
     * Factory method to create an ARGB instance from separate, specified red, green, blue, and alpha components.
     * Parameter values are derived from the least significant byte.  Integer values that range outside of [0-255] may
     * give unexpected results.  For values taken from user input, sensors, or otherwise uncertain sources, consider using
     * the factory method in the Color companion object.
     *
     * @see [[ai.dragonfly.color.ARGB.getIfValid]] for a method of constructing ARGB objects with input validation.
     * @param alpha integer value from [0-255] representing the alpha component in ARGB space.  Defaults to 255.
     * @param red   integer value from [0-255] representing the red component in RGB space.
     * @param green integer value from [0-255] representing the green component in RGB space.
     * @param blue  integer value from [0-255] representing the blue component in RGB space.
     * @return an instance of the ARGB case class.
     * @example {{{ val c = ARGB32(72,105,183) }}}
     */
    def apply(alpha: Int, red: Int, green: Int, blue: Int): ARGB32 = apply((alpha << 24) | (red << 16) | (green << 8) | blue)

    /**
     * Factory method to create a fully Opaque ARGB color; one with an alpha value of 255.
     * Because this method validates each intensity, it sacrifices performance
     * for safety.  Although well suited for parsing color data generated by sensors or user input, this method undercuts
     * performance in applications like reading image data.
     *
     * To skip validation and minimize overhead, @see [[ai.dragonfly.color.ARGB.apply]]
     *
     * @param red   integer value from [0-255] representing the red component in RGB space.
     * @param green integer value from [0-255] representing the green component in RGB space.
     * @param blue  integer value from [0-255] representing the blue component in RGB space.
     * @return an instance of the ARGB class or None if fed invalid input.
     */
    def getIfValid(red: Int, green: Int, blue: Int): Option[ARGB32] = getIfValid(255, red, green, blue)

    /**
     * Factory method to create an ARGB color.  Because this method validates each intensity, it sacrifices performance
     * for safety.  Although well suited for parsing color data generated by sensors or user input, this method undercuts
     * performance in applications like reading image data.
     *
     * To skip validation and minimize overhead, @see [[ai.dragonfly.color.ARGB.apply]]
     *
     * @param alpha integer value from [0-255] representing the alpha component in ARGB space.
     * @param red   integer value from [0-255] representing the red component in RGB space.
     * @param green integer value from [0-255] representing the green component in RGB space.
     * @param blue  integer value from [0-255] representing the blue component in RGB space.
     * @return an instance of the C class or None if fed invalid input.
     */
    def getIfValid(alpha: Int, red: Int, green: Int, blue: Int): Option[ARGB32] = {
      if (valid(alpha, red, green, blue)) Some(apply(alpha, red, green, blue))
      else None
    }

    override inline def clamp(red: Double, green: Double, blue: Double): Int = clamp(MAX, red, green, blue)

    override def fromXYZ(xyz: XYZ): ARGB32 = fromRGB(xyz.toRGB)

    override def fromRGB(rgb: RGB): ARGB32 = apply(clamp(rgb.red * MAX, rgb.green * MAX, rgb.blue * MAX))

    override def weightedAverage(c1: ARGB32, w1: Double, c2: ARGB32, w2: Double): ARGB32 = ARGB32(
      ((c1.alpha * w1) + (c2.alpha * w2)).toInt,
      ((c1.red * w1) + (c2.red * w2)).toInt,
      ((c1.green * w1) + (c2.green * w2)).toInt,
      ((c1.blue * w1) + (c2.blue * w2)).toInt
    )

    /**
     * Generate an ARGB instance from a single value.  This method validates the intensity parameter at some cost to performance.
     *
     * @param intensity the intensity of the desired gray value ranging from [0-255].
     * @return an ARGB instance encoding the desired grayscale intensity.
     */
    def grayIfValid(intensity: Int): Option[ARGB32] = {
      if (RGB.valid0to1(intensity)) Some(apply(intensity, intensity, intensity))
      else None
    }

    /**
     * Use Color.random() to obtain a random color in the form of an ARGB instance.
     * This method executes quickly and without memory costs, but the RGB color space biases toward cool colors.
     * In contrast, the Color.randomFromLabSpace() method takes seconds to initialize and has a memory footprint of several megabytes
     * However, it samples from a perceptually uniform color space and avoids the bias toward cool colors.
     * This method samples the Red, Green, and Blue color components uniformly, but always returns 255 for the alpha component.
     *
     * @return a randomly generated color sampled from the RGB Color Space.
     */
    override def random(r: scala.util.Random = Random.defaultRandom): ARGB32 = 0xFF000000 | r.nextInt(0xFFFFFF)
  }


  /**
   * ARGB is the primary case class for representing colors in ARGB space.
   *
   * @constructor Create a new ARGB object from an Int.
   * @see [[https://en.wikipedia.org/wiki/RGB_color_space]] for more information on the RGB color space.
   * @param argb a 32 bit integer that represents this color in ARGB space.
   *             The most significant byte encodes the alpha value, the second most significant byte encodes red,
   *             the third most significant byte encodes green, and the least significant byte encodes blue.
   * @return an instance of the ARGB case class.
   * @example {{{
   * val c = ARGB32(-1)  // returns fully opaque white
   * c.toString()  // returns "ARGB32(255,255,255,255)"
   * ARGB32(0xFF0000FF).toString() // returns "ARGB32(255,0,0,255)"
   * }}}
   */
  case class ARGB32(argb: Int) extends DiscreteRGB[ARGB32] {
    /**
     * @return the alpha component of this color in ARGB space.
     */
    inline def alpha: Int = argb >> 24 & 0xff

    /**
     * @return the red component of this color in ARGB space.
     */
    inline def red: Int = argb >> 16 & 0xff

    /**
     * @return the green component of this color in ARGB space.
     */
    inline def green: Int = argb >> 8 & 0xff

    /**
     * @return the blue component of this color in ARGB space.
     */
    inline def blue: Int = argb & 0xff

    override def toRGB: RGB = {
      import ARGB32.MAXD
      RGB(red.toDouble / MAXD, green.toDouble / MAXD, blue.toDouble / MAXD)
    }

    override def similarity(that: ARGB32): Double = ARGB32.similarity(this, that)

    /**
     * @return the hashcode.  For all color types, the hashcode function returns the same result as argb
     */
    override def hashCode(): Int = argb

    /**
     * @return true if these colors are equal in ARGB32 space, false otherwise
     */
    override def equals(obj: Any): Boolean = obj match {
      case that: ARGB32 => this.argb == that.argb
      case _ => false
    }

    /**
     * @return a hexadecimal string representing the rgba integer for this color.
     * @example {{{
     * val c = ARGB32(72,105,183)
     * c.hex() // returns "ff4869b7"
     * }}}
     */
    def hex(): String = Integer.toHexString(argb)

    /**
     * @return a string representing the color in an html friendly way.
     * @example {{{
     * val c = ARGB32(72,105,183)
     * c.html() // returns "#4869b7"
     * }}}
     */
    def html(): String = "#" + Integer.toHexString(argb | 0xff000000).substring(2)

    /**
     * @return a string representing the color in an SVG friendly way.
     * @example {{{
     * val c = ARGB32(72,105,183)
     * c.svg() // returns "rgb(72,105,183)"
     * }}}
     *
     * if the color has an alpha value less than 255, in other words, if the color has any measure of translucency,
     * this method returns an rgba svg string instead of an rgb string.
     * @example {{{
     * val c = ARGB32(72,105,183, 128)
     * c.svg() // returns "rgba(72,105,183,0.501960813999176)"
     * }}}
     */
    def svg(): String = {
      if (alpha < 255) s"rgba($red, $green, $blue, ${alpha / 255.0})"
      else s"rgb($red, $green, $blue)"
    }

    /**
     * @return a string representing the color in a CSS friendly way.
     * @example {{{
     * val c = ARGB32(72,105,183)
     * c.css() // returns "rgb(72,105,183)"
     * }}}
     *
     * if the color has an alpha value less than 255, in other words, if the color has any measure of translucency,
     * this method returns an rgba svg string instead of an rgb string.
     * @example {{{
     * val c = ARGB32(72,105,183, 128)
     * c.svg() // returns "rgba(72,105,183,0.501960813999176)"
     * }}}
     */
    def css: () => String = svg

    override def toString: String = s"ARGB32($alpha, $red, $green, $blue)"
  }


}
