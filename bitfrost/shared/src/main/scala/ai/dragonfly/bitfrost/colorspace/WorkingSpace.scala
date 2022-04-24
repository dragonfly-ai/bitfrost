package ai.dragonfly.bitfrost.colorspace

import Jama.Matrix
import ai.dragonfly.math.*
import vector.{Vector2, Vector3, VectorValues, dimensionCheck}
import matrix.*
import matrix.util.*
import ai.dragonfly.math.matrix.util.given_Dimensioned_Matrix
import ai.dragonfly.math.matrix.util.asColumnMatrix
import ai.dragonfly.bitfrost.*
import ai.dragonfly.bitfrost.colormodel


trait WorkingSpace extends colormodel.ARGB with colormodel.NRGB {

  val compander: Compander
  val primaries: ChromaticityPrimaries
  val illuminant: Illuminant

  lazy val M: Matrix = primaries.getM(illuminant)

  lazy val M_inverse: Matrix = M.inverse()

  import ai.dragonfly.bitfrost

  trait CommonColor[C <: CommonColor[C]] extends ColorVector[C] {
    def toNRGB: NRGB
  }

  trait CommonColorSpace[C <: CommonColor[C]] extends ColorVectorSpace[C] {
    //  def fromARGB(argb: ARGB):C
    def fromNRGB(nrgb: NRGB): C
  }

  trait PerceptualColor[C <: PerceptualColor[C]] extends ColorVector[C] {
    def toXYZ: XYZ
  }

  trait PerceptualColorSpace[C <: PerceptualColor[C]] extends ColorVectorSpace[C] {
    def fromXYZ(xyz: XYZ): C
  }

}

