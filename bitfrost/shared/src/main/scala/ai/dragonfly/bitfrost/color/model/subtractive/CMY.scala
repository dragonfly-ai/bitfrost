package ai.dragonfly.bitfrost.color.model.subtractive

import ai.dragonfly.bitfrost.cie.WorkingSpace
import ai.dragonfly.bitfrost.visualization.VolumeMesh
import ai.dragonfly.bitfrost.{ColorContext, NormalizedValue}
import ai.dragonfly.math.Random
import ai.dragonfly.math.vector.{Vector3, VectorValues, dimensionCheck}

trait CMY { self: WorkingSpace =>

  object CMY extends VectorSpace[CMY] with NormalizedValue {

    override val maxDistanceSquared: Double = 4.0

    def apply(values: VectorValues): CMY = new CMY(dimensionCheck(values, 3))

    def apply(cyan: Double, magenta: Double, yellow: Double): CMY = apply(VectorValues(cyan, magenta, yellow))


    /**
     * Factory method for creating instances of the CMY class.
     * This method validates input parameters at the cost of some performance.
     *
     * @param cyan    a value between [0-1]
     * @param magenta a value between [0-1]
     * @param yellow  a value between [0-1]
     * @return an instance of the CMY class.
     */
    def getIfValid(cyan: Double, magenta: Double, yellow: Double): Option[CMY] = {
      if (valid0to1(cyan, magenta, yellow)) Some(apply(cyan, magenta, yellow))
      else None
    }

    override def random(r: scala.util.Random = Random.defaultRandom): CMY = apply(
      VectorValues(
        r.nextDouble(),
        r.nextDouble(),
        r.nextDouble()
      )
    )

    override def fromXYZ(xyz: XYZ): CMY = fromRGB(xyz.toRGB)

    def fromRGB(rgb: RGB): CMY = apply(
      clamp0to1(
      1.0 - rgb.red,
      1.0 - rgb.green,
      1.0 - rgb.blue
      )
    )

    override lazy val gamut: VolumeMesh = VolumeMesh.cube()
  }

  /**
   * CMY is the primary case class for representing colors in CMY space.
   *
   * @constructor Create a new CMY object from three Double values.  This constructor does not validate input parameters.
   *              For values taken from user input, sensors, or otherwise uncertain sources, consider using the factory method in the Color companion object.
   * @see [[ai.dragonfly.color.CMY.getIfValid]] for a method of constructing CMY objects that validates inputs.
   * @see [[https://en.wikipedia.org/wiki/CMY_color_model]] for more information about the CMY color space.
   * @param cyan    a value ranging from [0-1].  Values outside of this range may cause errors.
   * @param magenta a value ranging from [0-1].  Values outside of this range may cause errors.
   * @param yellow  a value ranging from [0-1].  Values outside of this range may cause errors.
   * @return an instance of the CMY case class.
   * @example {{{
   * val c = CMY(1f, 0.25f, 0.5f, 0f)
   * c.toString()  // returns "CMY(1.000,0.250,0.500,0.000)"
   * }}}
   */

  case class CMY private(override val values: VectorValues) extends VectorModel[CMY] {
    override type VEC = this.type with CMY

    inline def cyan: Double = values(0)

    inline def magenta: Double = values(1)

    inline def yellow: Double = values(2)

    override def toXYZ: XYZ = toRGB.toXYZ

    override def toRGB: RGB = RGB.apply(
      RGB.clamp0to1(
        1.0 - cyan,
        1.0 - magenta,
        1.0 - yellow
      )
    )

    override def similarity(that: CMY): Double = CMY.similarity(this, that)

    override def toString: String = s"CMY($cyan, $magenta, $yellow)"

    override def copy(): VEC = new CMY(VectorValues(cyan, magenta, yellow)).asInstanceOf[VEC]
  }

}
