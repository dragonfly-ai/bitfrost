package ai.dragonfly.bitfrost.color.model.rgb.discrete

import ai.dragonfly.bitfrost.*
import ai.dragonfly.bitfrost.cie.WorkingSpace
import ai.dragonfly.bitfrost.color.*
import ai.dragonfly.bitfrost.color.model.*
import ai.dragonfly.mesh.*
import ai.dragonfly.mesh.shape.*
import ai.dragonfly.math.squareInPlace
import ai.dragonfly.math.vector.Vector3



trait DiscreteRGB {
  self: WorkingSpace =>
  
  trait DiscreteRGB[C <: DiscreteRGB[C]] extends DiscreteModel[C] {
    def red: Int

    def green: Int

    def blue: Int
  }


  trait UtilDiscreteRGB[C <: DiscreteRGB[C]] extends DiscreteSpace[C] {
    val min: Int = 0
    val MAX: Int
    lazy val MAXD: Double = MAX.toDouble

    override lazy val maxDistanceSquared: Double = 3 * squareInPlace(MAXD)

    override def distanceSquared(c1: C, c2: C): Double = squareInPlace(c1.red - c2.red) + squareInPlace(c1.green - c2.green) + squareInPlace(c1.blue - c2.blue)

    inline def valid(intensity: Int): Boolean = intensity >= min && intensity <= MAX

    inline def valid(i0: Int, i1: Int, i2: Int): Boolean = valid(i0) && valid(i1) && valid(i2)

    inline def valid(i0: Int, i1: Int, i2: Int, i3: Int): Boolean = valid(i0) && valid(i1) && valid(i2) && valid(i3)


    /**
     * Generate an C instance from a single value, skipping all overhead and validation.  Not suited for intensity data
     * provided by users, sensors, or other unreliable sources.
     *
     * @param intensity the intensity of the desired gray value ranging from [0-65535].
     * @return an ARGB instance encoding the desired grayscale intensity.
     */
    def gray(intensity: Int): C = apply(intensity, intensity, intensity)

    lazy val Clear: C = apply(0, 0, 0, 0)
    lazy val Black: C = apply(0, 0, 0)
    lazy val White: C = apply(MAX, MAX, MAX)
    lazy val Gray: C = gray(MAX / 2)
    lazy val DarkGray: C = gray(MAX / 4)
    lazy val LightGray: C = gray((3 * MAX) / 4)

    override def fromVector3(v: Vector3): C = apply(v.x.toInt, v.y.toInt, v.z.toInt)

    override def toVector3(c: C): Vector3 = Vector3(c.red, c.green, c.blue)

    // abstract
    def apply(red: Int, green: Int, blue: Int): C

    def apply(c1: Int, c2: Int, c3: Int, c4: Int): C

  }


  trait UtilRGB32[C <: DiscreteRGB[C]] extends UtilDiscreteRGB[C] {
    override val MAX: Int = 255

    inline def clamp(intensity: Double): Int = Math.round(Math.max(0.0, Math.min(MAX, intensity))).toInt

    inline def clamp(c4: Double, c3: Double, c2: Double, c1: Double): Int = {
      (clamp(c4) << 24) | (clamp(c3) << 16) | (clamp(c2) << 8) | clamp(c1)
    }

    // abstract
    inline def clamp(red: Double, green: Double, blue: Double): Int
  }


  trait UtilDiscreteRGB64[C <: DiscreteRGB[C]] extends UtilDiscreteRGB[C] {
    override val MAX: Int = 65535

    inline def clamp(intensity: Double): Long = Math.round(Math.max(0.0, Math.min(MAX, intensity)))

    inline def clamp(c4: Double, c3: Double, c2: Double, c1: Double): Long = {
      (clamp(c4) << 48) | (clamp(c3) << 32) | (clamp(c2) << 16) | clamp(c1)
    }

    /**
     * Generate an ARGB instance from a single value.  This method validates the intensity parameter at some cost to performance.
     *
     * @param intensity the intensity of the desired gray value ranging from [0-65535].
     * @return an ARGB instance encoding the desired grayscale intensity.
     */
    def grayIfValid(intensity: Int): Option[C] = {
      if (valid(intensity)) Some(apply(intensity, intensity, intensity))
      else None
    }

    // abstract
    def clamp(red: Double, green: Double, blue: Double): Long
  }

}