package ai.dragonfly.bitfrost.cie

import narr.*
import ai.dragonfly.math.vector.*
import util.*
import ai.dragonfly.math.matrix
import matrix.*
import ai.dragonfly.math.matrix.util.given_Dimensioned_Matrix
import ai.dragonfly.math.matrix.util.asColumnMatrix
import ai.dragonfly.math.matrix.ml.data.*

import scala.language.implicitConversions

object ChromaticAdaptation {

  // Chromatic Adaptation Matrices from  http://www.brucelindbloom.com/index.html?Eqn_ChromAdapt.html

  lazy val XYZ_Scaling: Matrix = Matrix.identity(3, 3)
  lazy val XYZ_Scaling_Inverse: Matrix = XYZ_Scaling

  lazy val Bradford: Matrix = Matrix(
    NArray[NArray[Double]](
      NArray[Double](0.8951, 0.2664, -0.1614),
      NArray[Double](-0.7502, 1.7135, 0.0367),
      NArray[Double](0.0389, -0.0685, 1.0296)
    )
  )

  lazy val Bradford_Inverse: Matrix = Matrix(
    NArray[NArray[Double]](
      NArray[Double](0.9869929, -0.1470543, 0.1599627),
      NArray[Double](0.4323053, 0.5183603, 0.0492912),
      NArray[Double](-0.0085287, 0.0400428, 0.9684867)
    )
  )

  lazy val Von_Kries: Matrix = Matrix(
    NArray[NArray[Double]](
      NArray[Double](0.40024, 0.7076, -0.08081),
      NArray[Double](-0.2263, 1.16532, 0.0457),
      NArray[Double](0.0, 0.0, 0.91822)
    )
  )
  lazy val Von_Kries_Inverse: Matrix = Matrix(
    NArray[NArray[Double]](
      NArray[Double](1.8599364, -1.1293816, 0.2198974),
      NArray[Double](0.3611914, 0.6388125, -0.0000064),
      NArray[Double](0.0, 0.0, 1.0890636)
    )
  )

}

case class ChromaticAdaptation[S <: WorkingSpace, T <: WorkingSpace](source:S, target:T, m:Matrix = Bradford) {

  val s:NArray[Double] = (m * source.illuminant.asColumnMatrix).getRowPackedCopy()

  val t:NArray[Double] = (m * target.illuminant.asColumnMatrix).getRowPackedCopy()

  val M:Matrix = m.inverse().times(Matrix(
    NArray[NArray[Double]](
      NArray[Double]( t(0) / s(0), 0.0, 0.0),
      NArray[Double]( 0.0, t(1) / s(1), 0.0),
      NArray[Double]( 0.0, 0.0, t(2) / s(2))
    )).times(m)
  )

  def apply(xyz:source.XYZ):target.XYZ = target.XYZ((M * Vector3(xyz.values).asColumnMatrix).getRowPackedCopy())
}