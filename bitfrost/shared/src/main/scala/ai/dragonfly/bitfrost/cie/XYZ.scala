package ai.dragonfly.bitfrost.cie

import ai.dragonfly.bitfrost.*
import ai.dragonfly.math.*
import ai.dragonfly.math.vector.*
import matrix.util.*
import matrix.util.given_Dimensioned_Matrix

import scala.language.implicitConversions

/**
 * From: https://en.wikipedia.org/wiki/CIE_1931_color_space
 * "The CIE XYZ color space encompasses all color sensations that are visible to a person with average eyesight.
 * That is why CIE XYZ (Tristimulus values) is a device-invariant representation of color."
 *
 * "In the CIE 1931 model, Y is the luminance, Z is quasi-equal to blue (of CIE RGB), and X is a mix of the three CIE RGB
 * curves chosen to be nonnegative."
 *
 *  "... the Z value is solely made up of the S cone response, the Y value a mix of L and M responses, and X value a mix
 *  of all three. This fact makes XYZ values analogous to, but different from, the LMS cone responses of the human eye."
 */

object XYZ {
  
  def toNRGB(workingSpace: WorkingSpace)(xyz:XYZ):workingSpace.NRGB = {
    val temp:VectorValues = (workingSpace.M_inverse * xyz.asColumnMatrix).getRowPackedCopy()
    for (i <- temp.indices) temp(i) = workingSpace.compander.encode(temp(i))
    workingSpace.NRGB(temp)
  }

  //def toARGB(workingSpace: WorkingSpace)(xyz:XYZ):workingSpace.ARGB = toNRGB(workingSpace)(xyz).toARGB.asInstanceOf[workingSpace.ARGB]

}
