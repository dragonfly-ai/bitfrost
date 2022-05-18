package ai.dragonfly.bitfrost.color.space

import ai.dragonfly.bitfrost.cie.WorkingSpace
import ai.dragonfly.math.squareInPlace
import ai.dragonfly.bitfrost.color.model.perceptual.XYZ
import ai.dragonfly.math.stats.geometry.Tetrahedron
import ai.dragonfly.math.stats.probability.distributions.stream.StreamingVectorStats
import ai.dragonfly.math.vector.{Vector3, VectorValues}

import ai.dragonfly.math.matrix.util.asColumnMatrix
import ai.dragonfly.math.matrix.util.given_Dimensioned_Matrix

import scala.collection.immutable

/**
 * From: https://en.wikipedia.org/wiki/CIE_1931_color_space
 * "The CIE XYZ color space encompasses all color sensations that are visible to a person with average eyesight.
 * That is why CIE XYZ (Tristimulus values) is a device-invariant representation of color."
 *
 * "In the CIE 1931 model, Y is the luminance, Z is quasi-equal to blue (of CIE RGB), and X is a mix of the three CIE RGB
 * curves chosen to be nonnegative."
 *
 * "... the Z value is solely made up of the S cone response, the Y value a mix of L and M responses, and X value a mix
 * of all three. This fact makes XYZ values analogous to, but different from, the LMS cone responses of the human eye."
 */

object XYZ {

  def toRGB(workingSpace: WorkingSpace)(xyz: XYZ): workingSpace.RGB = {
    val temp: VectorValues = (workingSpace.M_inverse * xyz.asColumnMatrix).getRowPackedCopy()
    for (i <- temp.indices) temp(i) = workingSpace.transferFunction.encode(temp(i))
    workingSpace.RGB(temp)
  }

  //def toARGB(workingSpace: WorkingSpace)(xyz:XYZ):workingSpace.ARGB = toNRGB(workingSpace)(xyz).toARGB.asInstanceOf[workingSpace.ARGB]

}
