package ai.dragonfly.bitfrost.cie

import Jama.Matrix
import ai.dragonfly.math.*
import vector.{VECTORS, Vector, Vector2, Vector3, VectorValues, dimensionCheck}
import matrix.*
import matrix.util.*
import ai.dragonfly.math.matrix.util.given_Dimensioned_Matrix
import ai.dragonfly.math.matrix.util.asColumnMatrix
import ai.dragonfly.math.matrix.data.StaticUnsupervisedData
import ai.dragonfly.bitfrost.color.model.*
import ai.dragonfly.bitfrost.color.model.perceptual.XYZ
import ai.dragonfly.bitfrost.color.model.rgb.discrete.{ARGB32, RGBA32}
import ai.dragonfly.bitfrost.color.model.rgb.RGB
import ai.dragonfly.bitfrost.color.spectral.{DEFAULT, Sample, SampleSet}
import ai.dragonfly.math.stats.geometry.Tetrahedron
import ai.dragonfly.math.stats.probability.distributions.Sampleable
import ai.dragonfly.math.stats.probability.distributions.stream.StreamingVectorStats

import java.io.PrintWriter
import scala.collection.{immutable, mutable}
import scala.util.Random


trait WorkingSpace extends XYZ with RGB with Gamut {

  //println("initializing WorkingSpace")

  val transferFunction: TransferFunction
  val primaries: ChromaticityPrimaries
  val illuminant: Illuminant

  lazy val whitePoint:XYZ = XYZ(illuminant.whitePointValues)

  lazy val M: Matrix = primaries.getM(illuminant)

  lazy val M_inverse: Matrix = M.inverse()

  given ctx:WorkingSpace = this

  /**
   * base trait from which all color model types inherit.
   */

  trait Model[C <: Model[C]] extends Color[C] {
    def toRGB: RGB
    def toXYZ: XYZ
  }

  trait DiscreteModel[C <: DiscreteModel[C]] extends Model[C] {
    override def toXYZ: XYZ = toRGB.toXYZ
  }

  trait CylindricalModel[C <: CylindricalModel[C]] extends Model[C] {
    val values:VectorValues
  }

  trait VectorModel[C <: VectorModel[C]] extends Model[C] with Vector {
    val values:VectorValues
  }

  trait PerceptualModel[C <: PerceptualModel[C]] extends VectorModel[C] {
  }

  trait LStarModel[C <: PerceptualModel[C]] extends PerceptualModel[C] {
  }

  //println("defined model traits")

  /**
   * Space traits for companion objects of Color Models.
   */

  trait Space[C <: Model[C]](using ctx:WorkingSpace) extends Sampleable[C] {
    type COLOR = C
    /**
     * Computes a weighted average of two colors in C color space.
     * @param c1 the first color.
     * @param w1 the weight of the first color in the range of [0-1].
     * @param c2 the second color.
     * @param w2 the weight of the second color in the range of [0-1].
     * @return the weighted average: c1 * w1 + c2 * w2.
     */
    def weightedAverage(c1: C, w1: Double, c2: C, w2: Double): C

    def maxDistanceSquared:Double

    def similarity(c1: C, c2: C): Double

    def fromRGB(rgb:RGB):C
    def fromXYZ(xyz:XYZ):C

    // Should every space have gamut volumes?
  }

  trait DiscreteSpace[C <: DiscreteModel[C]] extends Space[C] {

  }

  trait CylindricalSpace[C <: CylindricalModel[C]] extends Space[C] {

  }

  trait VectorSpace[C <: VectorModel[C]] extends Space[C] {

    /**
     * Computes a weighted average of two colors in C color space.
     * @param c1 the first color.
     * @param w1 the weight of the first color in the range of [0-1].
     * @param c2 the second color.
     * @param w2 the weight of the second color in the range of [0-1].
     * @return the weighted average: c1 * w1 + c2 * w2.
     */
    def weightedAverage(c1: C, w1: Double, c2: C, w2: Double): C = ((c1 * w1) + (c2 * w2)).asInstanceOf[C]

    override def similarity(c1: C, c2: C): Double = {
      1.0 - Math.sqrt(c1.euclid.distanceSquaredTo(c2) / maxDistanceSquared)
    }
  }


  trait PerceptualSpace[C <: PerceptualModel[C]] extends VectorSpace[C] {

    def apply(values: VectorValues): C

    def apply(c1: Double, c2: Double, c3: Double): C

    override def fromRGB(rgb: RGB): C = fromXYZ(rgb.toXYZ)

    def fullGamut:Gamut

    override lazy val maxDistanceSquared:Double = fullGamut.maxDistSquared

    def rgbGamut:Gamut

    override def random(r: Random = ai.dragonfly.math.Random.defaultRandom): C = {
      val v = rgbGamut.random(r)
      apply(v.x, v.y, v.z)
    }

  }

  trait LStarSpace[C <: PerceptualModel[C]] extends PerceptualSpace[C] {
    lazy val fullGamut:Gamut = Gamut.fromSpectralSamples(
      DEFAULT,
      (v:Vector3) => {
        Vector3(
          fromXYZ(
            XYZ(
              whitePoint.x * v.x,
              whitePoint.y * v.y,
              whitePoint.z * v.z
            )
          ).values
        )
      }
    )

    override lazy val maxDistanceSquared:Double = fullGamut.maxDistSquared

    override lazy val rgbGamut:Gamut = Gamut.fromRGB(transform = (xyz:XYZ) => Vector3(fromXYZ(xyz).values))


    //  override def random(r: Random = ai.dragonfly.math.Random.defaultRandom): C = {
    //    val v = fullGamut.random(r)
    //    apply(v.x, v.y, v.z)
    //  }

  }

  override def toString: String = this.getClass.getSimpleName.replace('$', '_')

}