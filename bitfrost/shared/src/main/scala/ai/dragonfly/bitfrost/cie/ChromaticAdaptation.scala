package ai.dragonfly.bitfrost.cie

import Jama.Matrix
import ai.dragonfly.math.matrix
import matrix.*
import ai.dragonfly.math.vector.*
import util.*
import matrix.util.given_Dimensioned_Matrix

import scala.language.implicitConversions

object ChromaticAdaptation {

  // Chromatic Adaptation Matrices from  http://www.brucelindbloom.com/index.html?Eqn_ChromAdapt.html

  lazy val XYZ_Scaling: Matrix = Matrix.identity(3, 3)
  lazy val XYZ_Scaling_Inverse: Matrix = XYZ_Scaling

  lazy val Bradford: Matrix = {
    val mvs = new MatrixValues(3)
    mvs(0) = VectorValues(0.8951, 0.2664, -0.1614)
    mvs(1) = VectorValues(-0.7502, 1.7135, 0.0367)
    mvs(2) = VectorValues(0.0389, -0.0685, 1.0296)
    new Matrix(mvs)
  }
  lazy val Bradford_Inverse: Matrix = {
    val mvs = new MatrixValues(3)
    mvs(0) = VectorValues(0.9869929, -0.1470543, 0.1599627)
    mvs(1) = VectorValues(0.4323053, 0.5183603, 0.0492912)
    mvs(2) = VectorValues(-0.0085287, 0.0400428, 0.9684867)
    new Matrix(mvs)
  }

  lazy val Von_Kries: Matrix = {
    val mvs = new MatrixValues(3)
    mvs(0) = VectorValues(0.40024, 0.7076, -0.08081)
    mvs(1) = VectorValues(-0.2263, 1.16532, 0.0457)
    mvs(2) = VectorValues(0.0, 0.0, 0.91822)
    new Matrix(mvs)
  }
  lazy val Von_Kries_Inverse: Matrix = new Matrix(
    MatrixValues(
      VectorValues(1.8599364, -1.1293816, 0.2198974),
      VectorValues(0.3611914, 0.6388125, -0.0000064),
      VectorValues(0.0, 0.0, 1.0890636)
    )
  )

}

class ChromaticAdaptation(sourceWhite:Illuminant, targetWhite:Illuminant, m:Matrix) {
  val s:VectorValues = (m * sourceWhite.vector.asColumnMatrix).getRowPackedCopy()
  val t:VectorValues = (m * targetWhite.vector.asColumnMatrix).getRowPackedCopy()
  val M = m.inverse().times(new Matrix(
    MatrixValues(
      VectorValues( t(0) / s(0), 0.0, 0.0),
      VectorValues( 0.0, t(1) / s(1), 0.0),
      VectorValues( 0.0, 0.0, t(2) / s(2))
    )).times(m)
  )

  def apply(xyz:Vector3):Vector3 = Vector3((M * xyz.asColumnMatrix).getRowPackedCopy())
}