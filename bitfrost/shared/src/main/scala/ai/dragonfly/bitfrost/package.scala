package ai.dragonfly

import Jama.Matrix

import ai.dragonfly.math
import ai.dragonfly.math.vector.*
import ai.dragonfly.math.matrix.*
import ai.dragonfly.math.matrix.MatrixValues
import ai.dragonfly.math.stats.probability.distributions.Sampleable


import scala.language.postfixOps
import scala.language.implicitConversions

package object bitfrost {

  type XYZ = Vector3

  extension (d: Double)
    inline def ⭑ : Double = d  // working stars: ✱ ✶ ✴

  object Degree {
    inline def valid(angle: Double): Boolean = angle >= 0f && angle <= 360.0
    inline def clamp(angle: Double): Double = (if (angle < 0.0) 360.0 + (angle % 360.0) else angle) % 360.0
  }

  object Hue {
    export Degree.*
    /*
     * For HSL, HSV, and CMYK conversion formulas:
     * http://www.rapidtables.com/convert/color/rgb-to-hsl.htm
     * http://www.rapidtables.com/convert/color/rgb-to-hsv.htm
    */


    inline def hueMinMax(red:Double, green: Double, blue:Double):VectorValues = {

      val min:Double = Math.min(red, Math.min(green, blue))
      val MAX:Double = Math.max(red, Math.max(green, blue))

      val delta:Double = MAX - min

      val h:Double = (if (delta == 0.0) {
        0
      } else {
        (60.0 * (
          if (red == MAX) {
            ((green - blue) / delta) % 6
          } else if (green == MAX) {
            (blue - red / delta) + 2
          } else {
            ((red - green) / delta) + 4
          }
          )) + 360
      }) % 360

      VectorValues(h, min, MAX)
    }

    inline def toHSV(red:Double, green: Double, blue:Double): VectorValues = {
      val values:VectorValues = hueMinMax(red, green, blue)
      values(1) = 100.0 * (values(2 /*MAX*/) - values(1 /*min*/)) / values(2 /*MAX*/)
      values(2) = 100.0 * values(2 /*MAX*/)
      values
    }

    inline def toHSL(red:Double, green: Double, blue:Double): VectorValues = {
      val values:VectorValues = hueMinMax(red, green, blue)
      values(2 /*MAX*/)
      val delta:Double = values(2 /*MAX*/) - values(1 /*min*/)
      val L:Double = (values(2 /*MAX*/) + values(1 /*min*/)) / 2.0
      val denom:Double = 1.0 - Math.abs(2.0 * L - 1.0)
      values(1) = 100.0 * (if (denom <= 0.0) 0.0 else delta / denom)
      values(2) = 100.0 * L
      values
    }

    inline def hcxmToNRGBvalues(hue: Double, c: Double, x: Double, m: Double): VectorValues = {
      val X = m + x
      val C = m + c
      val Z = m

      // rotate/clamp hue between 0 and 360
      val h = clamp(hue)

      if (h < 60.0) NRGB.clamp(C, X, Z)
      else if (h < 120.0) NRGB.clamp(X, C, Z)
      else if (h < 180.0) NRGB.clamp(Z, C, X)
      else if (h < 240.0) NRGB.clamp(Z, X, C)
      else if (h < 300.0) NRGB.clamp(X, Z, C)
      else NRGB.clamp(C, Z, X)
    }

    inline def hcToX(H: Double, C: Double): Double = {
      val hh = H/60.0
      C * ( 1 - Math.abs( hh % 2 - 1 ) )
    }
  }

  object Percentage {
    inline def valid(percentage: Double): Boolean = percentage >= 0f && percentage <= 100f
    inline def clamp(percentage: Double): Double = Math.min(100.0, Math.max(0, percentage))
  }

  val Saturation:Percentage.type = Percentage

  val Value:Percentage.type = Percentage

  val Lightness:Percentage.type = Percentage

  object RGB {
    inline def valid(intensity: Int): Boolean = intensity >= 0 && intensity < 256
    inline def valid(i0: Int, i1: Int, i2: Int):Boolean = valid(i0) && valid(i1) && valid(i2)
    inline def valid(i0: Int, i1: Int, i2: Int, i3: Int):Boolean = valid(i0) && valid(i1) && valid(i2) && valid(i3)

    inline def clamp(intensity:Double):Int = Math.max(0, Math.min(255, intensity.toInt))
    inline def clamp(red: Double, green: Double, blue: Double):Int = clamp(255.0, red, green, blue)
    inline def clamp(alpha:Double, red: Double, green: Double, blue: Double):Int = {
      (clamp(alpha)<<24)|(clamp(red)<<16)|(clamp(green)<<8)|clamp(blue)
    }
  }

  object NRGB {
    inline def valid(intensity: Double): Boolean = intensity >= 0.0 && intensity <= 1.0
    inline def valid(i0: Double, i1: Double, i2: Double):Boolean = valid(i0) && valid(i1) && valid(i2)

    inline def clamp(intensity:Double):Double = Math.max(0.0, Math.min(1.0, intensity))
    inline def clamp(red: Double, green: Double, blue: Double):VectorValues = {
      VectorValues(clamp(red), clamp(green), clamp(blue))
    }
  }


  object context {

    import ai.dragonfly.bitfrost.cie.*
    import Illuminant.*

    //Adobe RGB (1998)
    // specification: https://www.adobe.com/digitalimag/pdfs/AdobeRGB1998.pdf
    object Adobe_RGB_1998 extends WorkingSpace {
      override val compander:Compander = Gamma(2.19921875)

      override val primaries:ChromaticityPrimaries = ChromaticityPrimaries(
        ChromaticityPrimary( Vector2(0.64, 0.33), 0.297361 ),
        ChromaticityPrimary( Vector2(0.21, 0.71), 0.627355 ),
        ChromaticityPrimary( Vector2(0.15, 0.06), 0.075285 )
      )

      override val illuminant: Illuminant = D65

      val verificationMatrix: Matrix = new Matrix(MatrixValues(
        VectorValues(1.93939393939394, 1.0, 0.090909090909091),
        VectorValues(0.295774647887324, 1.0, 0.112676056338028),
        VectorValues(2.5, 1.0, 13.1666666666667)
      ))
    }

    // Apple RGB
    object Apple_RGB extends WorkingSpace {
      override val compander:Compander = Gamma(1.8)

      override val primaries:ChromaticityPrimaries = ChromaticityPrimaries(
        ChromaticityPrimary( Vector2(0.625, 0.34), 0.244634 ),
        ChromaticityPrimary( Vector2(0.28, 0.595), 0.672034 ),
        ChromaticityPrimary( Vector2(0.155, 0.07), 0.083332 )
      )

      override val illuminant: Illuminant = D65

      val verificationMatrix: Matrix = new Matrix(MatrixValues(
        VectorValues(1.83823529411765, 1.0, 0.102941176470588),
        VectorValues(0.470588235294118, 1.0, 0.210084033613445),
        VectorValues(2.21428571428571, 1.0, 11.0714285714286)
      ))
    }
    // Best RGB
    object Best_RGB extends WorkingSpace {
      override val compander: Compander = Gamma(2.2)

      override val primaries: ChromaticityPrimaries = ChromaticityPrimaries(
        ChromaticityPrimary(Vector2(0.7347, 0.2653), 0.228457),
        ChromaticityPrimary(Vector2(0.215, 0.775), 0.737352),
        ChromaticityPrimary(Vector2(0.13, 0.035), 0.034191)
      )

      override val illuminant: Illuminant = D50

      val verificationMatrix: Matrix = new Matrix(MatrixValues(
        VectorValues(2.76931775348662, 1.0, 0.0),
        VectorValues(0.27741935483871, 1.0, 0.012903225806452),
        VectorValues(3.71428571428571, 1.0, 23.8571428571429)
      ))
    }

    object Beta_RGB extends WorkingSpace {
      override val compander: Compander = Gamma(2.2)

      override val primaries: ChromaticityPrimaries = ChromaticityPrimaries(
        ChromaticityPrimary(Vector2(0.6888, 0.3112), 0.303273),
        ChromaticityPrimary(Vector2(0.1986, 0.7551), 0.663786),
        ChromaticityPrimary(Vector2(0.1265, 0.0352), 0.032941)
      )

      override val illuminant: Illuminant = D50

      val verificationMatrix: Matrix = new Matrix(MatrixValues(
        VectorValues(2.2133676092545, 1.0, 0.0),
        VectorValues(0.263011521652761, 1.0, 0.061316381936167),
        VectorValues(3.59375, 1.0, 23.8153409090909)
      ))
    }

    object Bruce_RGB extends WorkingSpace {
      override val compander: Compander = Gamma(2.2)

      override val primaries: ChromaticityPrimaries = ChromaticityPrimaries(
        ChromaticityPrimary(Vector2(0.64, 0.33), 0.240995),
        ChromaticityPrimary(Vector2(0.28, 0.65), 0.683554),
        ChromaticityPrimary(Vector2(0.15, 0.06), 0.075452)
      )

      override val illuminant: Illuminant = D65

      val verificationMatrix: Matrix = new Matrix(MatrixValues(
        VectorValues(1.93939393939394, 1.0, 0.090909090909091),
        VectorValues(0.430769230769231, 1.0, 0.107692307692308),
        VectorValues(2.5, 1.0, 13.1666666666667)
      ))
    }

    object CIE_RGB extends WorkingSpace {
      override val compander: Compander = Gamma(2.2)

      override val primaries: ChromaticityPrimaries = ChromaticityPrimaries(
        ChromaticityPrimary(Vector2(0.735, 0.265), 0.176204),
        ChromaticityPrimary(Vector2(0.274, 0.717), 0.812985),
        ChromaticityPrimary(Vector2(0.167, 0.009), 0.010811)
      )

      override val illuminant: Illuminant = E

      val verificationMatrix: Matrix = new Matrix(MatrixValues(
        VectorValues(2.77358490566038, 1.0, 0.0),
        VectorValues(0.382147838214784, 1.0, 0.01255230125523),
        VectorValues(18.5555555555556, 1.0, 91.5555555555556)
      ))
    }

    object ColorMatch_RGB extends WorkingSpace {
      override val compander: Compander = Gamma(1.8)

      override val primaries: ChromaticityPrimaries = ChromaticityPrimaries(
        ChromaticityPrimary(Vector2(0.63, 0.34), 0.274884),
        ChromaticityPrimary(Vector2(0.295, 0.605), 0.658132),
        ChromaticityPrimary(Vector2(0.15, 0.075), 0.066985)
      )

      override val illuminant: Illuminant = D50

      val verificationMatrix: Matrix = new Matrix(MatrixValues(
        VectorValues(1.85294117647059, 1.0, 0.088235294117647),
        VectorValues(0.487603305785124, 1.0, 0.165289256198347),
        VectorValues(2, 1.0, 10.3333333333333)
      ))
    }

    object Don_RGB_4 extends WorkingSpace {
      override val compander: Compander = Gamma(2.2)

      override val primaries: ChromaticityPrimaries = ChromaticityPrimaries(
        ChromaticityPrimary(Vector2(0.696, 0.3), 0.27835),
        ChromaticityPrimary(Vector2(0.215, 0.765), 0.68797),
        ChromaticityPrimary(Vector2(0.13, 0.035), 0.03368)
      )

      override val illuminant: Illuminant = D50

      val verificationMatrix: Matrix = new Matrix(MatrixValues(
        VectorValues(2.32, 1.0, 0.013333333333333),
        VectorValues(0.281045751633987, 1.0, 0.026143790849673),
        VectorValues(3.71428571428571, 1.0, 23.8571428571429)
      ))
    }

    object ECI_RGB_v2 extends WorkingSpace {
      override val compander: Compander = Lstar

      override val primaries: ChromaticityPrimaries = ChromaticityPrimaries(
        ChromaticityPrimary(Vector2(0.67, 0.33), 0.32025),
        ChromaticityPrimary(Vector2(0.21, 0.71), 0.602071),
        ChromaticityPrimary(Vector2(0.14, 0.08), 0.077679)
      )

      override val illuminant: Illuminant = D50

      val verificationMatrix: Matrix = new Matrix(MatrixValues(
        VectorValues(2.03030303030303, 1.0, 0.0),
        VectorValues(0.295774647887324, 1.0, 0.112676056338028),
        VectorValues(1.75, 1.0, 9.75)
      ))
    }

    object Ekta_Space_PS5 extends WorkingSpace {
      override val compander: Compander = Gamma(2.2)

      override val primaries: ChromaticityPrimaries = ChromaticityPrimaries(
        ChromaticityPrimary(Vector2(0.695, 0.305), 0.260629),
        ChromaticityPrimary(Vector2(0.26, 0.7), 0.734946),
        ChromaticityPrimary(Vector2(0.11, 0.005), 0.004425)
      )

      override val illuminant: Illuminant = D50

      val verificationMatrix: Matrix = new Matrix(MatrixValues(
        VectorValues(2.27868852459016, 1.0, 0.0),
        VectorValues(0.371428571428571, 1.0, 0.057142857142857),
        VectorValues(22, 1.0, 177)
      ))
    }

    object NTSC_RGB extends WorkingSpace {
      override val compander: Compander = Gamma(2.2)
      override val primaries: ChromaticityPrimaries = ChromaticityPrimaries(
        ChromaticityPrimary(Vector2(0.67, 0.33), 0.298839),
        ChromaticityPrimary(Vector2(0.21, 0.71), 0.586811),
        ChromaticityPrimary(Vector2(0.14, 0.08), 0.11435)
      )

      override val illuminant: Illuminant = C

      val verificationMatrix: Matrix = new Matrix(MatrixValues(
        VectorValues(2.03030303030303, 1.0, 0.0),
        VectorValues(0.295774647887324, 1.0, 0.112676056338028),
        VectorValues(1.75, 1.0, 9.75)
      ))
    }

    // PAL/SECAM RGB
    object PAL_RGB extends WorkingSpace {
      override val compander: Compander = Gamma(2.2)

      override val primaries: ChromaticityPrimaries = ChromaticityPrimaries(
        ChromaticityPrimary(Vector2(0.64, 0.33), 0.222021),
        ChromaticityPrimary(Vector2(0.29, 0.6), 0.706645),
        ChromaticityPrimary(Vector2(0.15, 0.06), 0.071334)
      )

      override val illuminant: Illuminant = D65

      val verificationMatrix: Matrix = new Matrix(MatrixValues(
        VectorValues(1.93939393939394, 1.0, 0.090909090909091),
        VectorValues(0.483333333333333, 1.0, 0.183333333333333),
        VectorValues(2.5, 1.0, 13.1666666666667)
      ))
    }

    val SECAM_RGB = PAL_RGB

    object ProPhoto_RGB extends WorkingSpace {
      override val compander: Compander = Gamma(1.8)

      override val primaries: ChromaticityPrimaries = ChromaticityPrimaries(
        ChromaticityPrimary(Vector2(0.7347, 0.2653), 0.28804),
        ChromaticityPrimary(Vector2(0.1596, 0.8404), 0.711874),
        ChromaticityPrimary(Vector2(0.0366, 0.0001), 8.6E-05)
      )

      override val illuminant: Illuminant = D50

      val verificationMatrix: Matrix = new Matrix(MatrixValues(
        VectorValues(2.76931775348662, 1.0, 0.0),
        VectorValues(0.189909566872918, 1.0, 0.0),
        VectorValues(366.0, 1.0, 9633.0)
      ))
    }

    // SMPTE-C RGB
    object SMPTE_Minus_C_RGB extends WorkingSpace {
      override val compander: Compander = Gamma(2.2)

      override val primaries: ChromaticityPrimaries = ChromaticityPrimaries(
        ChromaticityPrimary(Vector2(0.63, 0.34), 0.212395),
        ChromaticityPrimary(Vector2(0.31, 0.595), 0.701049),
        ChromaticityPrimary(Vector2(0.155, 0.07), 0.086556)
      )

      override val illuminant: Illuminant = D65

      val verificationMatrix: Matrix = new Matrix(MatrixValues(
        VectorValues(1.85294117647059, 1.0, 0.088235294117647),
        VectorValues(0.521008403361345, 1.0, 0.159663865546218),
        VectorValues(2.21428571428571, 1.0, 11.0714285714286)
      ))
    }

    object sRGB extends WorkingSpace {
      override val compander: Compander = SRGB // ~2.2 ?

      override val primaries: ChromaticityPrimaries = ChromaticityPrimaries(
        ChromaticityPrimary(Vector2(0.64, 0.33), 0.212656),
        ChromaticityPrimary(Vector2(0.3, 0.6), 0.715158),
        ChromaticityPrimary(Vector2(0.15, 0.06), 0.072186)
      )

      override val illuminant: Illuminant = D65

      val verificationMatrix: Matrix = new Matrix(MatrixValues(
        VectorValues(1.93939393939394, 1.0, 0.090909090909091),
        VectorValues(0.5, 1.0, 0.166666666666667),
        VectorValues(2.5, 1.0, 13.1666666666667)
      ))
    }

    object Wide_Gamut_RGB extends WorkingSpace {
      override val compander: Compander = Gamma(2.2)

      override val primaries: ChromaticityPrimaries = ChromaticityPrimaries(
        ChromaticityPrimary(Vector2(0.735, 0.265), 0.258187),
        ChromaticityPrimary(Vector2(0.115, 0.826), 0.724938),
        ChromaticityPrimary(Vector2(0.157, 0.018), 0.016875)
      )

      override val illuminant: Illuminant = D50

      val verificationMatrix: Matrix = new Matrix(MatrixValues(
        VectorValues(2.77358490566038, 1.0, 0.0),
        VectorValues(0.139225181598063, 1.0, 0.071428571428571),
        VectorValues(8.72222222222222, 1.0, 45.8333333333333)
      ))
    }

  }

}
