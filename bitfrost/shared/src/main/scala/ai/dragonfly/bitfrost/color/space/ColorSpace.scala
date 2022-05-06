package ai.dragonfly.bitfrost.color.space

import ai.dragonfly.bitfrost.*
import ai.dragonfly.bitfrost.cie.{GamutXYZ, WorkingSpace, XYZ}
import ai.dragonfly.bitfrost.color.*
import ai.dragonfly.bitfrost.color.model.*
import ai.dragonfly.bitfrost.color.model.rgb.RGB
import ai.dragonfly.bitfrost.color.model.rgb.discrete.ARGB32
import ai.dragonfly.math.stats.probability.distributions.Sampleable
import ai.dragonfly.math.vector.Vector3

import scala.collection.mutable
import scala.util.Random

//type ColorSpace[CM <: ColorModel, WS <: WorkingSpace] = CM with WS

//type sRGB = ColorSpace[RGB, ai.dragonfly.bitfrost.context.sRGB.type]

/**
 * trait for Color Companion Objects.
 */

trait ColorSpace[C <: ColorModel[C], WS <: WorkingSpace] extends Sampleable[C] {
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

trait DiscreteColorSpace[C <: DiscreteColorModel[C], WS <: WorkingSpace] extends ColorSpace[C, WS] {

}

trait CylindricalColorSpace[C <: CylindricalColorModel[C], WS <: WorkingSpace] extends ColorSpace[C, WS] {

}

trait VectorColorSpace[C <: VectorColorModel[C], WS <: WorkingSpace] extends ColorSpace[C, WS] {

  /**
   * Computes a weighted average of two colors in C color space.
   * @param c1 the first color.
   * @param w1 the weight of the first color in the range of [0-1].
   * @param c2 the second color.
   * @param w2 the weight of the second color in the range of [0-1].
   * @return the weighted average: c1 * w1 + c2 * w2.
   */
  def weightedAverage(c1: C, w1: Double, c2: C, w2: Double): C = ((c1 * w1) + (c2 * w2)).asInstanceOf[C]

  override def similarity(c1: C, c2: C): Double = c1.euclid.distanceSquaredTo(c2) / maxDistanceSquared
}


trait PerceptualColorSpace[C <: PerceptualColorModel[C], WS <: WorkingSpace] extends VectorColorSpace[C, WS] {

  def fromXYZ(xyz: XYZ): C

  def apply(c1:Double, c2:Double, c3:Double): C

  lazy val tetrahedralVolume:TetrahedralVolume = XYZ.tetrahedralVolume(GamutXYZ.CuratedCIE2006_5nm, (v:Vector3) => { val o = Vector3(fromXYZ(v).values); println(o); o })

  override def random(r: Random = ai.dragonfly.math.Random.defaultRandom): C = {
    val v = tetrahedralVolume.random(r)
    apply(v.x, v.y, v.z)
  }

}
