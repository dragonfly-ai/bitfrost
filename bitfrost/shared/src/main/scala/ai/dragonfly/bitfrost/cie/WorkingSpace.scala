package ai.dragonfly.bitfrost.cie

import Jama.Matrix
import ai.dragonfly.math.*
import vector.{Vector2, Vector3, VectorValues, dimensionCheck}
import matrix.*
import matrix.util.*
import ai.dragonfly.math.matrix.util.given_Dimensioned_Matrix
import ai.dragonfly.math.matrix.util.asColumnMatrix
import ai.dragonfly.bitfrost.*
import ai.dragonfly.bitfrost.color.model.*
import ai.dragonfly.bitfrost.color.model.rgb.discrete.{ARGB32, RGBA32}
import ai.dragonfly.bitfrost.color.model.rgb.RGB
import ai.dragonfly.bitfrost.color.space.*
import ai.dragonfly.math.stats.geometry.Tetrahedron

import scala.collection.mutable


trait WorkingSpace extends ARGB32 with RGBA32 with RGB {

  val transferFunction: TransferFunction
  val primaries: ChromaticityPrimaries
  val illuminant: Illuminant

  lazy val M: Matrix = primaries.getM(illuminant)

  lazy val M_inverse: Matrix = M.inverse()

}