package ai.dragonfly

import Jama.Matrix
import ai.dragonfly.bitfrost.cie.WorkingSpace
import ai.dragonfly.bitfrost.color.model.*
import ai.dragonfly.bitfrost.color.model.huesat.{HSL, HSV}
import ai.dragonfly.bitfrost.color.model.perceptual.{Lab, Luv}
import ai.dragonfly.bitfrost.color.model.subtractive.{CMY, CMYK}
import ai.dragonfly.bitfrost.color.spectral.*
import ai.dragonfly.math
import ai.dragonfly.math.vector.*
import ai.dragonfly.math.matrix.*
import ai.dragonfly.math.matrix.MatrixValues
import ai.dragonfly.math.squareInPlace
import ai.dragonfly.math.stats.probability.distributions.Sampleable

package object bitfrost {

  trait NormalizedValue {

    inline def valid0to1(i: Double): Boolean = 0.0 <= i && i <= 1.0
    inline def valid0to1(i0: Double, i1: Double):Boolean = valid0to1(i0) && valid0to1(i1)
    inline def valid0to1(i0: Double, i1: Double, i2: Double):Boolean = valid0to1(i0) && valid0to1(i1) && valid0to1(i2)
    inline def valid0to1(i0: Double, i1: Double, i2: Double, i3: Double):Boolean = valid0to1(i0) && valid0to1(i1) && valid0to1(i2) && valid0to1(i3)

    inline def clamp0to1(i: Double): Double = Math.min(1.0, Math.max(0.0, i))
    inline def clamp0to1(i0: Double, i1: Double, i2: Double):VectorValues = {
      VectorValues(clamp0to1(i0), clamp0to1(i1), clamp0to1(i2))
    }
    inline def clamp0to1(i0: Double, i1: Double, i2: Double, i3: Double):VectorValues = {
      VectorValues(clamp0to1(i0), clamp0to1(i1), clamp0to1(i2), clamp0to1(i3))
    }
  }

  trait ProvidedColorContexts extends WorkingSpace
    with rgb.RGB
    with rgb.discrete.ARGB32
    with rgb.discrete.ARGB64
    with rgb.discrete.RGBA32
    with rgb.discrete.RGBA64
    with CMY
    with CMYK
    with HSL
    with HSV
    with Lab
    with Luv

  object ColorContext {

    import ai.dragonfly.bitfrost.cie.*
    import Illuminant.*

    val knownContexts:Array[ProvidedColorContexts] = Array[ProvidedColorContexts](
      Adobe_RGB_1998,
      Apple_RGB,
      Best_RGB,
      Beta_RGB,
      Bruce_RGB,  // included as a gesture of gratitude to Bruce Lindenbloom who's work inspired this library and made it possible.
      CIE_RGB,
      ColorMatch_RGB,
      Don_RGB_4,
      ECI_RGB_v2,
      Ekta_Space_PS5,
      NTSC_RGB,
      PAL_RGB,
      ProPhoto_RGB,
      SMPTE_C_RGB,
      sRGB,
      Wide_Gamut_RGB,
      P3_D65_Display
    )

    //Adobe RGB (1998)
    // specification: https://www.adobe.com/digitalimag/pdfs/AdobeRGB1998.pdf
    object Adobe_RGB_1998 extends ProvidedColorContexts {
      override val transferFunction:TransferFunction = Gamma(2.19921875)

      override val primaries:ChromaticityPrimaries = ChromaticityPrimaries(
        ChromaticityPrimary( 0.64, 0.33, 0.297361 ),
        ChromaticityPrimary( 0.21, 0.71, 0.627355 ),
        ChromaticityPrimary( 0.15, 0.06, 0.075285 )
      )

      override val illuminant: Illuminant = D65

      val verificationMatrix: Matrix = new Matrix(MatrixValues(
        VectorValues(1.93939393939394, 1.0, 0.090909090909091),
        VectorValues(0.295774647887324, 1.0, 0.112676056338028),
        VectorValues(2.5, 1.0, 13.1666666666667)
      ))
    }
    // Apple RGB
    object Apple_RGB extends ProvidedColorContexts {
      override val transferFunction:TransferFunction = Gamma(1.8)

      override val primaries:ChromaticityPrimaries = ChromaticityPrimaries(
        ChromaticityPrimary( 0.625, 0.34, 0.244634 ),
        ChromaticityPrimary( 0.28, 0.595, 0.672034 ),
        ChromaticityPrimary( 0.155, 0.07, 0.083332 )
      )

      override val illuminant: Illuminant = D65

      val verificationMatrix: Matrix = new Matrix(MatrixValues(
        VectorValues(1.83823529411765, 1.0, 0.102941176470588),
        VectorValues(0.470588235294118, 1.0, 0.210084033613445),
        VectorValues(2.21428571428571, 1.0, 11.0714285714286)
      ))
    }
    // Best RGB
    object Best_RGB extends ProvidedColorContexts {
      override val transferFunction: TransferFunction = Gamma(2.2)

      override val primaries: ChromaticityPrimaries = ChromaticityPrimaries(
        ChromaticityPrimary( 0.7347, 0.2653, 0.228457),
        ChromaticityPrimary( 0.215, 0.775, 0.737352),
        ChromaticityPrimary( 0.13, 0.035, 0.034191)
      )

      override val illuminant: Illuminant = D50

      val verificationMatrix: Matrix = new Matrix(MatrixValues(
        VectorValues(2.76931775348662, 1.0, 0.0),
        VectorValues(0.27741935483871, 1.0, 0.012903225806452),
        VectorValues(3.71428571428571, 1.0, 23.8571428571429)
      ))
    }

    object Beta_RGB extends ProvidedColorContexts {
      override val transferFunction: TransferFunction = Gamma(2.2)

      override val primaries: ChromaticityPrimaries = ChromaticityPrimaries(
        ChromaticityPrimary( 0.6888, 0.3112, 0.303273),
        ChromaticityPrimary( 0.1986, 0.7551, 0.663786),
        ChromaticityPrimary( 0.1265, 0.0352, 0.032941)
      )

      override val illuminant: Illuminant = D50

      val verificationMatrix: Matrix = new Matrix(MatrixValues(
        VectorValues(2.2133676092545, 1.0, 0.0),
        VectorValues(0.263011521652761, 1.0, 0.061316381936167),
        VectorValues(3.59375, 1.0, 23.8153409090909)
      ))
    }

    object Bruce_RGB extends ProvidedColorContexts {
      override val transferFunction: TransferFunction = Gamma(2.2)

      override val primaries: ChromaticityPrimaries = ChromaticityPrimaries(
        ChromaticityPrimary( 0.64, 0.33, 0.240995),
        ChromaticityPrimary( 0.28, 0.65, 0.683554),
        ChromaticityPrimary( 0.15, 0.06, 0.075452)
      )

      override val illuminant: Illuminant = D65

      val verificationMatrix: Matrix = new Matrix(MatrixValues(
        VectorValues(1.93939393939394, 1.0, 0.090909090909091),
        VectorValues(0.430769230769231, 1.0, 0.107692307692308),
        VectorValues(2.5, 1.0, 13.1666666666667)
      ))
    }

    object CIE_RGB extends ProvidedColorContexts {
      override val transferFunction: TransferFunction = Gamma(2.2)

      override val primaries: ChromaticityPrimaries = ChromaticityPrimaries(
        ChromaticityPrimary( 0.735, 0.265, 0.176204),
        ChromaticityPrimary( 0.274, 0.717, 0.812985),
        ChromaticityPrimary( 0.167, 0.009, 0.010811)
      )

      override val illuminant: Illuminant = E

      val verificationMatrix: Matrix = new Matrix(MatrixValues(
        VectorValues(2.77358490566038, 1.0, 0.0),
        VectorValues(0.382147838214784, 1.0, 0.01255230125523),
        VectorValues(18.5555555555556, 1.0, 91.5555555555556)
      ))
    }

    object ColorMatch_RGB extends ProvidedColorContexts {
      override val transferFunction: TransferFunction = Gamma(1.8)

      override val primaries: ChromaticityPrimaries = ChromaticityPrimaries(
        ChromaticityPrimary( 0.63, 0.34, 0.274884),
        ChromaticityPrimary( 0.295, 0.605, 0.658132),
        ChromaticityPrimary( 0.15, 0.075, 0.066985)
      )

      override val illuminant: Illuminant = D50

      val verificationMatrix: Matrix = new Matrix(MatrixValues(
        VectorValues(1.85294117647059, 1.0, 0.088235294117647),
        VectorValues(0.487603305785124, 1.0, 0.165289256198347),
        VectorValues(2, 1.0, 10.3333333333333)
      ))
    }

    object Don_RGB_4 extends ProvidedColorContexts {
      override val transferFunction: TransferFunction = Gamma(2.2)

      override val primaries: ChromaticityPrimaries = ChromaticityPrimaries(
        ChromaticityPrimary( 0.696, 0.3, 0.27835),
        ChromaticityPrimary( 0.215, 0.765, 0.68797),
        ChromaticityPrimary( 0.13, 0.035, 0.03368)
      )

      override val illuminant: Illuminant = D50

      val verificationMatrix: Matrix = new Matrix(MatrixValues(
        VectorValues(2.32, 1.0, 0.013333333333333),
        VectorValues(0.281045751633987, 1.0, 0.026143790849673),
        VectorValues(3.71428571428571, 1.0, 23.8571428571429)
      ))
    }

    object ECI_RGB_v2 extends ProvidedColorContexts {
      override val transferFunction: TransferFunction = Lstar

      override val primaries: ChromaticityPrimaries = ChromaticityPrimaries(
        ChromaticityPrimary( 0.67, 0.33, 0.32025),
        ChromaticityPrimary( 0.21, 0.71, 0.602071),
        ChromaticityPrimary( 0.14, 0.08, 0.077679)
      )

      override val illuminant: Illuminant = D50

      val verificationMatrix: Matrix = new Matrix(MatrixValues(
        VectorValues(2.03030303030303, 1.0, 0.0),
        VectorValues(0.295774647887324, 1.0, 0.112676056338028),
        VectorValues(1.75, 1.0, 9.75)
      ))
    }

    object Ekta_Space_PS5 extends ProvidedColorContexts {
      override val transferFunction: TransferFunction = Gamma(2.2)

      override val primaries: ChromaticityPrimaries = ChromaticityPrimaries(
        ChromaticityPrimary( 0.695, 0.305, 0.260629),
        ChromaticityPrimary( 0.26, 0.7, 0.734946),
        ChromaticityPrimary( 0.11, 0.005, 0.004425)
      )

      override val illuminant: Illuminant = D50

      val verificationMatrix: Matrix = new Matrix(MatrixValues(
        VectorValues(2.27868852459016, 1.0, 0.0),
        VectorValues(0.371428571428571, 1.0, 0.057142857142857),
        VectorValues(22, 1.0, 177)
      ))
    }

    // 1953 NTSC https://en.wikipedia.org/wiki/NTSC#Colorimetry
    object NTSC_RGB extends ProvidedColorContexts {
      override val transferFunction: TransferFunction = Gamma(2.2)
      override val primaries: ChromaticityPrimaries = ChromaticityPrimaries(
        ChromaticityPrimary( 0.67, 0.33, 0.298839),
        ChromaticityPrimary( 0.21, 0.71, 0.586811),
        ChromaticityPrimary( 0.14, 0.08, 0.11435)
      )

      override val illuminant: Illuminant = C

      val verificationMatrix: Matrix = new Matrix(MatrixValues(
        VectorValues(2.03030303030303, 1.0, 0.0),
        VectorValues(0.295774647887324, 1.0, 0.112676056338028),
        VectorValues(1.75, 1.0, 9.75)
      ))

      override val cmf: SampleSet = CIE1931_2deg_5nm
    }

    // PAL/SECAM RGB
    object PAL_RGB extends ProvidedColorContexts {
      override val transferFunction: TransferFunction = Gamma(2.2)

      override val primaries: ChromaticityPrimaries = ChromaticityPrimaries(
        ChromaticityPrimary( 0.64, 0.33, 0.222021),
        ChromaticityPrimary( 0.29, 0.6, 0.706645),
        ChromaticityPrimary( 0.15, 0.06, 0.071334)
      )

      override val illuminant: Illuminant = D65

      val verificationMatrix: Matrix = new Matrix(MatrixValues(
        VectorValues(1.93939393939394, 1.0, 0.090909090909091),
        VectorValues(0.483333333333333, 1.0, 0.183333333333333),
        VectorValues(2.5, 1.0, 13.1666666666667)
      ))
    }

    val SECAM_RGB = PAL_RGB

    object ProPhoto_RGB extends ProvidedColorContexts {
      override val transferFunction: TransferFunction = Gamma(1.8)

      override val primaries: ChromaticityPrimaries = ChromaticityPrimaries(
        ChromaticityPrimary( 0.7347, 0.2653, 0.28804),
        ChromaticityPrimary( 0.1596, 0.8404, 0.711874),
        ChromaticityPrimary( 0.0366, 0.0001, 8.6E-05)
      )

      override val illuminant: Illuminant = D50

      val verificationMatrix: Matrix = new Matrix(MatrixValues(
        VectorValues(2.76931775348662, 1.0, 0.0),
        VectorValues(0.189909566872918, 1.0, 0.0),
        VectorValues(366.0, 1.0, 9633.0)
      ))
    }

    // SMPTE "C" RGB: https://en.wikipedia.org/wiki/NTSC#SMPTE_C
    object SMPTE_C_RGB extends ProvidedColorContexts {
      override val transferFunction: TransferFunction = Gamma(2.2)

      override val primaries: ChromaticityPrimaries = ChromaticityPrimaries(
        ChromaticityPrimary( 0.63, 0.34, 0.212395),
        ChromaticityPrimary( 0.31, 0.595, 0.701049),
        ChromaticityPrimary( 0.155, 0.07, 0.086556)
      )

      override val illuminant: Illuminant = D65

      val verificationMatrix: Matrix = new Matrix(MatrixValues(
        VectorValues(1.85294117647059, 1.0, 0.088235294117647),
        VectorValues(0.521008403361345, 1.0, 0.159663865546218),
        VectorValues(2.21428571428571, 1.0, 11.0714285714286)
      ))

      override val cmf: SampleSet = CIE1931_2deg_5nm
    }

    object sRGB extends ProvidedColorContexts {
      override val transferFunction: TransferFunction = sRGB_ICC_V2 // ~2.2 ?

      override val primaries: ChromaticityPrimaries = ChromaticityPrimaries(
        ChromaticityPrimary( 0.64, 0.33, 0.212656),
        ChromaticityPrimary( 0.3, 0.6, 0.715158),
        ChromaticityPrimary( 0.15, 0.06, 0.072186)
      )

      override val illuminant: Illuminant = D65

      val verificationMatrix: Matrix = new Matrix(MatrixValues(
        VectorValues(1.93939393939394, 1.0, 0.090909090909091),
        VectorValues(0.5, 1.0, 0.166666666666667),
        VectorValues(2.5, 1.0, 13.1666666666667)
      ))
      override val cmf: SampleSet = CIE1931_JUDD1951_VOS1978_2deg_5nm
    }

    object Wide_Gamut_RGB extends ProvidedColorContexts {
      override val transferFunction: TransferFunction = Gamma(2.2)

      override val primaries: ChromaticityPrimaries = ChromaticityPrimaries(
        ChromaticityPrimary( 0.735, 0.265, 0.258187),
        ChromaticityPrimary( 0.115, 0.826, 0.724938),
        ChromaticityPrimary( 0.157, 0.018, 0.016875)
      )

      override val illuminant: Illuminant = D50

      val verificationMatrix: Matrix = new Matrix(MatrixValues(
        VectorValues(2.77358490566038, 1.0, 0.0),
        VectorValues(0.139225181598063, 1.0, 0.071428571428571),
        VectorValues(8.72222222222222, 1.0, 45.8333333333333)
      ))
    }

    object P3_D65_Display extends ProvidedColorContexts {
      //https://www.dcimovies.com/archives/spec_v1/DCI_Digital_Cinema_System_Spec_v1.pdf
      override val primaries: ChromaticityPrimaries = ChromaticityPrimaries(
        ChromaticityPrimary( 0.6800, 0.3200, Double.NaN),
        ChromaticityPrimary( 0.2650, 0.6900, Double.NaN),
        ChromaticityPrimary( 0.1500, 0.0600, Double.NaN)
      )
      //https://www.color.org/chardata/rgb/DisplayP3.xalter
      override val transferFunction: TransferFunction = sRGB_ICC_V2

      override val illuminant: Illuminant = D65
    }

  }

}
