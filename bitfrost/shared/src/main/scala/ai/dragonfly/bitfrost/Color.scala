package ai.dragonfly.bitfrost

import ai.dragonfly.bitfrost.cie.{WorkingSpace, XYZ}
import ai.dragonfly.math.stats.probability.distributions.Sampleable
import ai.dragonfly.math.vector.*

/**
 * Color is the base trait from which all other color types inherit.
 */

trait Color[C <: Color[C]] {
  def similarity(that:C):Double
}

trait DiscreteColor[C <: DiscreteColor[C]] extends Color[C] {
}

trait ColorVector[C <: ColorVector[C]] extends Color[C] with Vector {
  override def similarity(that: C): Double = this.euclid.distanceTo(that)

}

/**
 * trait for Color Companion Objects.
 */

trait ColorSpace[C <: Color[C]] extends Sampleable[C] {
  /**
   * Computes a weighted average of two colors in C color space.
   * @param c1 the first color.
   * @param w1 the weight of the first color in the range of [0-1].
   * @param c2 the second color.
   * @param w2 the weight of the second color in the range of [0-1].
   * @return the weighted average: c1 * w1 + c2 * w2.
   */
  def weightedAverage(c1: C, w1: Double, c2: C, w2: Double): C

}

trait DiscreteColorSpace[C <: DiscreteColor[C]] extends ColorSpace[C] {

}

trait ColorVectorSpace[C <: ColorVector[C]] extends ColorSpace[C] {

  /**
   * Computes a weighted average of two colors in C color space.
   * @param c1 the first color.
   * @param w1 the weight of the first color in the range of [0-1].
   * @param c2 the second color.
   * @param w2 the weight of the second color in the range of [0-1].
   * @return the weighted average: c1 * w1 + c2 * w2.
   */
  def weightedAverage(c1: C, w1: Double, c2: C, w2: Double): C = ((c1 * w1) + (c2 * w2)).asInstanceOf[C]

}
