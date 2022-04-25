package ai.dragonfly.bitfrost

package object color {


  import ai.dragonfly.bitfrost.*
  import ai.dragonfly.bitfrost.cie.{WorkingSpace, XYZ}
  import ai.dragonfly.bitfrost.color.model.*
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

  trait VectorColor[C <: VectorColor[C]] extends Color[C] with Vector {
    override def similarity(that: C): Double = this.euclid.distanceTo(that)

  }


  trait PerceptualColor[C <: PerceptualColor[C]] extends VectorColor[C] {
    def toXYZ: XYZ
  }

  trait PerceptualColorCompanion[C <: PerceptualColor[C]] extends VectorColorModelCompanion[C] {
    def fromXYZ(xyz: XYZ): C
  }


  /**
   * trait for Color Companion Objects.
   */

  trait ColorModelCompanion[C <: Color[C]] extends Sampleable[C] {
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

  trait DiscreteColorModelCompanion[C <: DiscreteColor[C]] extends ColorModelCompanion[C] {

  }

  trait VectorColorModelCompanion[C <: VectorColor[C]] extends ColorModelCompanion[C] {

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


}
