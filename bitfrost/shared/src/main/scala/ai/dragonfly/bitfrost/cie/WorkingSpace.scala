package ai.dragonfly.bitfrost.cie

import Jama.Matrix
import ai.dragonfly.math.*
import vector.{Vector2, Vector3, VectorValues, dimensionCheck}
import matrix.*
import matrix.util.*
import ai.dragonfly.math.matrix.util.given_Dimensioned_Matrix
import ai.dragonfly.math.matrix.util.asColumnMatrix
import ai.dragonfly.bitfrost.*
import ai.dragonfly.bitfrost.color.model.rgb.discrete.{ARGB32, RGBA32}
import ai.dragonfly.bitfrost.color.model.rgb.RGB
import ai.dragonfly.bitfrost.color.{VectorColor, VectorColorModelCompanion}


trait WorkingSpace extends ARGB32 with RGBA32 with RGB {

  val transferFunction: TransferFunction
  val primaries: ChromaticityPrimaries
  val illuminant: Illuminant

  lazy val M: Matrix = primaries.getM(illuminant)

  lazy val M_inverse: Matrix = M.inverse()

  import ai.dragonfly.bitfrost

  trait CommonColor[C <: CommonColor[C]] extends VectorColor[C] {
    def toRGB: RGB
  }

  trait CommonColorCompanion[C <: CommonColor[C]] extends VectorColorModelCompanion[C] {
    //  def fromARGB(argb: ARGB):C
    def fromRGB(nrgb: RGB): C
  }

}