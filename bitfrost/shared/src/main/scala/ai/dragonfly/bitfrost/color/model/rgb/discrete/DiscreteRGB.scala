package ai.dragonfly.bitfrost.color.model.rgb.discrete

import ai.dragonfly.bitfrost.*
import ai.dragonfly.bitfrost.color.*

trait DiscreteRGB[C <: DiscreteRGB[C]] extends DiscreteColor[C]


trait UtilDiscreteRGB[C <: DiscreteRGB[C]] extends DiscreteColorModelCompanion[C] {
  val min:Int
  val MAX:Int
  val MAXD:Double

  inline def valid(intensity: Int): Boolean = intensity >= min && intensity <= MAX
  inline def valid(i0: Int, i1: Int, i2: Int):Boolean = valid(i0) && valid(i1) && valid(i2)
  inline def valid(i0: Int, i1: Int, i2: Int, i3: Int):Boolean = valid(i0) && valid(i1) && valid(i2) && valid(i3)


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
  lazy val DargGray: C = gray(MAX / 4)
  lazy val LightGray: C = gray((3 * MAX) / 4)

  // abstract
  def apply(red:Int, green:Int, blue:Int):C
  def apply(c1:Int, c2:Int, c3:Int, c4:Int):C
}



trait UtilRGB32[C <: DiscreteRGB[C]] extends UtilDiscreteRGB[C] {
  override val min:Int = 0
  override val MAX:Int = 255
  override val MAXD:Double = 255.0

  inline def clamp(intensity:Double):Int = Math.round(Math.max(0.0, Math.min(MAX, intensity))).toInt
  inline def clamp(c4:Double, c3: Double, c2: Double, c1: Double):Int = {
    (clamp(c4)<<24)|(clamp(c3)<<16)|(clamp(c2)<<8)|clamp(c1)
  }

  // abstract
  inline def clamp(red: Double, green: Double, blue: Double):Int
}


trait UtilDiscreteRGB64[C <: DiscreteRGB[C]] extends UtilDiscreteRGB[C] {
  override val min:Int = 0
  override val MAX:Int = 65535
  override val MAXD:Double = 65535.0

  inline def clamp(intensity:Double):Long = Math.round(Math.max(0.0, Math.min(MAX, intensity)))
  inline def clamp(c4:Double, c3: Double, c2: Double, c1: Double):Long = {
    (clamp(c4)<<48)|(clamp(c3)<<32)|(clamp(c2)<<16)|clamp(c1)
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
  def clamp(red: Double, green: Double, blue: Double):Long
}
