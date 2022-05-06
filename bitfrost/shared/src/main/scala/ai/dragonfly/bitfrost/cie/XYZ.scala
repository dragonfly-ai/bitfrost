package ai.dragonfly.bitfrost.cie

import ai.dragonfly.bitfrost.*
import ai.dragonfly.bitfrost.cie.GamutXYZ.CuratedCIE2006_5nm
import ai.dragonfly.bitfrost.color.space.TetrahedralVolume
import ai.dragonfly.math.*
import ai.dragonfly.math.stats.geometry.Tetrahedron
import ai.dragonfly.math.stats.probability.distributions.stream.StreamingVectorStats
import ai.dragonfly.math.vector.*
import matrix.util.*
import matrix.util.given_Dimensioned_Matrix

import scala.collection.{immutable, mutable}
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

  def toRGB(workingSpace: WorkingSpace)(xyz:XYZ):workingSpace.RGB = {
    val temp:VectorValues = (workingSpace.M_inverse * xyz.asColumnMatrix).getRowPackedCopy()
    for (i <- temp.indices) temp(i) = workingSpace.transferFunction.encode(temp(i))
    workingSpace.RGB(temp)
  }

  //def toARGB(workingSpace: WorkingSpace)(xyz:XYZ):workingSpace.ARGB = toNRGB(workingSpace)(xyz).toARGB.asInstanceOf[workingSpace.ARGB]

  def tetrahedralVolume(spectralMatchingFunctions:Array[MatchingFunctions], transform:Vector3 => Vector3 = (v: Vector3) => v): TetrahedralVolume = {

    var sv:StreamingVectorStats = new StreamingVectorStats(3)

    val points: Array[Vector3] = new Array[Vector3](squareInPlace(spectralMatchingFunctions.length))
    var p:Int = 0
    for (i <- spectralMatchingFunctions.indices) {
      for (j <- spectralMatchingFunctions.indices) {
        val v:Vector3 = Vector3(0.0, 0.0, 0.0)
        for (k <- 0 to i) {
          v.add(spectralMatchingFunctions((j + k) % spectralMatchingFunctions.length).xyz)
        }
        points(p) = v
        sv(v)
        p += 1
      }
    }

    val sv2 = new StreamingVectorStats(3)

    for (i <- points.indices) {
      val xyz:Vector3 = points(i)
      val nxyz = Vector3(xyz.x / sv.maxValues(0), xyz.y / sv.maxValues(1), xyz.z / sv.maxValues(2))
//      print(s"xyz = $xyz => nxyz = $nxyz => transform(nxyz) = ${transform(nxyz)}")
      points(i) = transform(nxyz)
      sv2(points(i))
    }

    val mean:Vector3 = Vector3(sv2.average().values)

    println(s"mean $mean")

    println(s"point count: $p")

    val hEnd:Int = points.length - spectralMatchingFunctions.length
    var cumulativelyWeightedTrahedra:immutable.Seq[(Double, Tetrahedron)] = immutable.Seq[(Double, Tetrahedron)]()
    var t:Int = 0

    addTetrahedron( Tetrahedron(mean, points(0), points(1), points(spectralMatchingFunctions.length)) )

    def addTetrahedron(tet:Tetrahedron):Unit = {
      if (tet == null) println ("adding a null tetrahedron")
      else {
        var vol:Double = tet.volume
        if (vol > 0) {
          vol = cumulativelyWeightedTrahedra.lastOption match {
            case Some((d, _)) => d + vol
            case None => vol
          }
          cumulativelyWeightedTrahedra = cumulativelyWeightedTrahedra :+ (vol, tet)
        }
        t += 1
      }
    }

    while (t < spectralMatchingFunctions.length) addTetrahedron(Tetrahedron(mean, points(0), points(t + 1), points(t)))

    val end = ( 2 * ( points.length - 1 ) ) - spectralMatchingFunctions.length
    while (t < end) {
      val i:Int = (t - spectralMatchingFunctions.length) / 2
      addTetrahedron(
        if (i < hEnd) {
          val h: Int = i + spectralMatchingFunctions.length
          if (t % 2 == 1) Tetrahedron(mean, points(i), points(h), points(h - 1))
          else Tetrahedron(mean, points(i + 1), points(h), points(i))
        } else {
          val h: Int = points.length - 1
          Tetrahedron(mean, points(i), points(h), points(h - 1))
        }
      )
    }

    TetrahedralVolume(cumulativelyWeightedTrahedra)
  }

}

case class MatchingFunctions(λ:Double, xyz:Vector3)