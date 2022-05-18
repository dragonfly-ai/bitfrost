package ai.dragonfly.bitfrost.color.space

import ai.dragonfly.bitfrost.*
import ai.dragonfly.bitfrost.cie.{Illuminant, WorkingSpace}
import ai.dragonfly.bitfrost.color.*
import ai.dragonfly.bitfrost.color.model.*
import ai.dragonfly.bitfrost.color.model.perceptual.XYZ
import ai.dragonfly.bitfrost.color.model.rgb.RGB
import ai.dragonfly.bitfrost.color.model.rgb.discrete.ARGB32
import ai.dragonfly.bitfrost.color.spectrum.SpectralTables
import ai.dragonfly.math.stats.probability.distributions.Sampleable
import ai.dragonfly.math.vector.Vector3

import scala.collection.mutable
import scala.util.Random

//type ColorSpace[CM <: ColorModel] = CM with WS

//type sRGB = ColorSpace[RGB, ai.dragonfly.bitfrost.context.sRGB.type]

/**
 * trait for Color Companion Objects.
 */

trait ColorSpace[C <: ColorModel[C]] extends Sampleable[C] {
  /**
   * Computes a weighted average of two colors in C color space.
   * @param c1 the first color.
   * @param w1 the weight of the first color in the range of [0-1].
   * @param c2 the second color.
   * @param w2 the weight of the second color in the range of [0-1].
   * @return the weighted average: c1 * w1 + c2 * w2.
   */
  def weightedAverage(c1: C, w1: Double, c2: C, w2: Double): C

  val maxDistanceSquared:Double

  def similarity(c1: C, c2: C): Double

}

trait DiscreteColorSpace[C <: DiscreteColorModel[C]] extends ColorSpace[C] {

}

trait CylindricalColorSpace[C <: CylindricalColorModel[C]] extends ColorSpace[C] {

}

trait VectorColorSpace[C <: VectorColorModel[C]] extends ColorSpace[C] {

  /**
   * Computes a weighted average of two colors in C color space.
   * @param c1 the first color.
   * @param w1 the weight of the first color in the range of [0-1].
   * @param c2 the second color.
   * @param w2 the weight of the second color in the range of [0-1].
   * @return the weighted average: c1 * w1 + c2 * w2.
   */
  def weightedAverage(c1: C, w1: Double, c2: C, w2: Double): C = ((c1 * w1) + (c2 * w2)).asInstanceOf[C]

  override def similarity(c1: C, c2: C): Double = 1.0 - Math.sqrt(c1.euclid.distanceSquaredTo(c2) / maxDistanceSquared)
}


trait PerceptualColorSpace[C <: PerceptualColorModel[C]] extends VectorColorSpace[C] {

  def fromXYZ(xyz: XYZ): C

  def apply(c1:Double, c2:Double, c3:Double): C

  def ill:Illuminant

  lazy val fullGamut:Gamut = Gamut.fromSpectralSamples(
    SpectralTables.XYZ_5NM_WITH_0_1NM_PEAKS_CIE2006,
    (v:Vector3) => {
      Vector3(
        fromXYZ(
          Vector3(
            ill.vector.x * v.x,
            ill.vector.y * v.y,
            ill.vector.z * v.z
          )
        ).values
      )
    }
  )

  override val maxDistanceSquared:Double = fullGamut.maxDistSquared

  def rgbGamut:Gamut

  override def random(r: Random = ai.dragonfly.math.Random.defaultRandom): C = {
    val v = rgbGamut.random(r)
    apply(v.x, v.y, v.z)
  }

}
