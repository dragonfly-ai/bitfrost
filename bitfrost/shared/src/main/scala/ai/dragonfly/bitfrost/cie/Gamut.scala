package ai.dragonfly.bitfrost.cie

import ai.dragonfly.bitfrost.color.spectral.SampleSet
import ai.dragonfly.math.matrix.PCA
import ai.dragonfly.math.matrix.data.StaticUnsupervisedData
import ai.dragonfly.math.squareInPlace
import ai.dragonfly.math.stats.geometry.Tetrahedron
import ai.dragonfly.math.stats.probability.distributions.Sampleable
import ai.dragonfly.math.stats.probability.distributions.stream.StreamingVectorStats
import ai.dragonfly.math.vector.{VECTORS, Vector3}

import java.io.PrintWriter
import scala.collection.immutable

trait Gamut { self: WorkingSpace =>
  object Gamut {
//    println("Gamut")

    def apply(cumulativelyWeightedTrahedra: immutable.Seq[(Double, Tetrahedron)], maxDistSquared: Double): Gamut = {

      val totalVolume: Double = cumulativelyWeightedTrahedra.last._1
      //println(s"TetrahedralVolume.apply(...): totalVolume = $totalVolume")

      val cumulative: Array[Double] = new Array[Double](cumulativelyWeightedTrahedra.size)
      val tetrahedra: Array[Tetrahedron] = new Array[Tetrahedron](cumulativelyWeightedTrahedra.size)
      var remaining: immutable.Seq[(Double, Tetrahedron)] = cumulativelyWeightedTrahedra
      var i: Int = 0
      while (remaining.nonEmpty) {
        val t = remaining.head
        cumulative(i) = t._1 / totalVolume
        tetrahedra(i) = t._2
        remaining = remaining.tail
        i += 1
      }
      Gamut(tetrahedra, cumulative, maxDistSquared)
    }

    def computeMaxDistSquared(points: Array[Vector3], mean: Vector3): Double = {
      val vs: VECTORS = new VECTORS(points.length)

      for (i <- points.indices) vs(i) = points(i) - mean

      val pca = PCA(StaticUnsupervisedData(vs))

      val mode = pca.basisPairs.head.basisVector

      var min: Double = Double.MaxValue
      var minV: Vector3 = mean
      var MAX: Double = Double.MinValue
      var vMAX: Vector3 = mean

      points.foreach {
        p =>
          val t: Double = mode dot p
          if (t < min) {
            min = t
            minV = p
          }
          if (t > MAX) {
            MAX = t
            vMAX = p
          }
      }

      minV.euclid.distanceSquaredTo(vMAX)

    }

    def fromRGB(res: Double = 0.025, transform: XYZ => Vector3 = (v: XYZ) => Vector3(v.values)): Gamut = {

      var c: Double = 0

      var cumulativelyWeightedTrahedra: immutable.Seq[(Double, Tetrahedron)] = immutable.Seq[(Double, Tetrahedron)]()

      val svs: StreamingVectorStats = StreamingVectorStats(3)

      val size: Int = squareInPlace(1.0 / res).toInt * 24

      val points: Array[Vector3] = new Array[Vector3](size)
      var p: Int = 0

      def addPoints(p0: Vector3, p1: Vector3, p2: Vector3, p3: Vector3): Unit = {
        svs(p0)(p1)(p2)(p3)

        points(p) = p0
        points(p + 1) = p1
        points(p + 2) = p2
        points(p + 3) = p3

        p = p + 4
      }

      while (c < 1.0) {

        var i: Double = 0
        while (i < 1.0) {
          addPoints(
            transform(RGB(1.0, c, i).toXYZ),
            transform(RGB(1.0, c + res, i).toXYZ),
            transform(RGB(1.0, c, i + res).toXYZ),
            transform(RGB(1.0, c + res, i + res).toXYZ)
          )

          addPoints(
            transform(RGB(0.0, c, i).toXYZ),
            transform(RGB(0.0, c + res, i).toXYZ),
            transform(RGB(0.0, c, i + res).toXYZ),
            transform(RGB(0.0, c + res, i + res).toXYZ)
          )

          addPoints(
            transform(RGB(c, 1.0, i).toXYZ),
            transform(RGB(c + res, 1.0, i).toXYZ),
            transform(RGB(c, 1.0, i + res).toXYZ),
            transform(RGB(c + res, 1.0, i + res).toXYZ)
          )

          addPoints(
            transform(RGB(c, 0.0, i).toXYZ),
            transform(RGB(c + res, 0.0, i).toXYZ),
            transform(RGB(c, 0.0, i + res).toXYZ),
            transform(RGB(c + res, 0.0, i + res).toXYZ)
          )

          addPoints(
            transform(RGB(c, i, 1.0).toXYZ),
            transform(RGB(c + res, i, 1.0).toXYZ),
            transform(RGB(c, i + res, 1.0).toXYZ),
            transform(RGB(c + res, i + res, 1.0).toXYZ)
          )

          addPoints(
            transform(RGB(c, i, 0.0).toXYZ),
            transform(RGB(c + res, i, 0.0).toXYZ),
            transform(RGB(c, i + res, 0.0).toXYZ),
            transform(RGB(c + res, i + res, 0.0).toXYZ)
          )

          i += res
        }
        c += res
      }

      //    println(s"p = $p vs size = $size")
      val center: Vector3 = Vector3(svs.average().values)

      val maxDistSquared: Double = computeMaxDistSquared(points, center)

      def addTetrahedron(p0: Vector3, p1: Vector3, p2: Vector3, p3: Vector3): Unit = {
        addTet(Tetrahedron(center, p0, p1, p2))
        addTet(Tetrahedron(center, p3, p2, p1))
      }

      def addTet(tet: Tetrahedron): Unit = {
        if (tet == null) println("tried to add a null tetrahedron?")
        else {
          var vol: Double = tet.volume
          if (vol > 0) {
            vol = cumulativelyWeightedTrahedra.lastOption match {
              case Some((d, _)) => d + vol
              case None => vol
            }
            cumulativelyWeightedTrahedra = cumulativelyWeightedTrahedra :+ (vol, tet)
          }
        }
      }

      for (i <- 0 until points.length by 4) {
        addTetrahedron(
          points(i),
          points(i + 1),
          points(i + 2),
          points(i + 3)
        )
      }

      apply(cumulativelyWeightedTrahedra, maxDistSquared)
    }

    def fromSpectralSamples(spectralSamples: SampleSet, illuminant: Illuminant): Gamut = fromSpectralSamples(
      spectralSamples,
      (v: Vector3) => Vector3(
        v.x * illuminant.xₙ,
        v.y * illuminant.yₙ,
        v.z * illuminant.zₙ,
      )
    )


    def fromSpectralSamples(spectralSamples: SampleSet, transform: Vector3 => Vector3 = (v: Vector3) => v): Gamut = {

      val sv2 = new StreamingVectorStats(3)

      val points: Array[Vector3] = new Array[Vector3](spectralSamples.volumePoints.length)

      for (i <- points.indices) {
        points(i) = transform(spectralSamples.volumePoints(i))
        sv2(points(i))
      }

      val mean: Vector3 = Vector3(sv2.average().values)

      val maxDistSquared: Double = computeMaxDistSquared(points, mean)

      val hEnd: Int = points.length - spectralSamples.sampleCount
      var cumulativelyWeightedTrahedra: immutable.Seq[(Double, Tetrahedron)] = immutable.Seq[(Double, Tetrahedron)]()
      var t: Int = 0

      addTet(Tetrahedron(mean, points(0), points(1), points(spectralSamples.sampleCount)))

      def addTet(tet: Tetrahedron): Unit = {
        if (tet == null) println("tried to add a null tetrahedron?")
        else {
          var vol: Double = tet.volume
          if (vol > 0) {
            vol = cumulativelyWeightedTrahedra.lastOption match {
              case Some((d, _)) => d + vol
              case None => vol
            }
            cumulativelyWeightedTrahedra = cumulativelyWeightedTrahedra :+ (vol, tet)
          }
//          else {
//            println("tried to add a zero volume tetrahedron?")
//          }
          t += 1
        }
      }

      // black adjacent:
      while (t < spectralSamples.sampleCount) addTet(Tetrahedron(mean, points(0), points(t + 1), points(t)))

      val end = (2 * (points.length - 1)) - spectralSamples.sampleCount
      while (t < end) {
        val i: Int = (t - spectralSamples.sampleCount) / 2
        addTet(
          if (i < hEnd) {
            val h: Int = i + spectralSamples.sampleCount
            if (t % 2 == 1) Tetrahedron(mean, points(i), points(h), points(h - 1))
            else Tetrahedron(mean, points(i + 1), points(h), points(i))
          } else {
            val h: Int = points.length - 1
            Tetrahedron(mean, points(i), points(h), points(h - 1))
          }
        )
      }

      // white adjacent:
      for (i <- (points.length - 1) - spectralSamples.sampleCount until points.length - 2) addTet(Tetrahedron(mean, points(i), points(i + 1), points.last))

      Gamut(cumulativelyWeightedTrahedra, maxDistSquared)
    }


    def writePLY(gamut: Gamut, transform: Vector3 => ai.dragonfly.bitfrost.ColorContext.sRGB.ARGB32, out: java.io.OutputStream): Unit = {
      val sout: PrintWriter = new PrintWriter(out)

      sout.write(
        s"""ply
format ascii 1.0
comment generated by bitfrost: https://github.com/dragonfly-ai/bitfrost
comment Mesh reprsentation of Color Space $this
element vertex ${gamut.tetrahedra.length * 3}
property float x
property float y
property float z
property uchar red
property uchar green
property uchar blue
property uchar alpha
element face ${gamut.tetrahedra.length}
property list uchar uint vertex_indices
end_header
"""
      )

      def writeVertex(v: Vector3): Unit = {
        val c: ai.dragonfly.bitfrost.ColorContext.sRGB.ARGB32 = transform(v)
        // y and z swapped for blender and three.js
        sout.write(s"${v.z} ${v.y} ${v.x} ${c.red} ${c.green} ${c.blue} 255\n")
      }

      var vi: Int = 0
      var fi: Int = 0

      val sbFaces = StringBuilder()
      var nullTets = 0
      for (tetrahedron <- gamut.tetrahedra) {

        if (tetrahedron == null) nullTets += 1
        else {
          tetrahedron.vertices.tail.foreach((v: Vector3) => writeVertex(v))

          sbFaces.append(s"3 $vi ${vi + 1} ${vi + 2}\n")

          fi += 1

          vi += 3
        }
      }

//      println(s"nullTets = $nullTets vs length ${gamut.tetrahedra.length}")

      sout.write(sbFaces.toString())
      sout.flush()

    }

//    println("defined Gamut object methods")
  }


  /**
   *
   * @param tetrahedra
   * @param cumulative
   */

  case class Gamut private(tetrahedra: Array[Tetrahedron], cumulative: Array[Double], maxDistSquared: Double) extends Sampleable[Vector3] {

    val mean: Vector3 = tetrahedra(0).v1

    private def getNearestIndex(target: Double): Int = {
      var left = 0
      var right = cumulative.length - 1
      while (left < right) {
        val mid = (left + right) / 2
        if (cumulative(mid) < target) left = mid + 1
        else if (cumulative(mid) > target) right = mid - 1
        else return mid
      }
      right
    }

    override def random(r: scala.util.Random = ai.dragonfly.math.Random.defaultRandom): Vector3 = {
      val x = r.nextDouble()
      val i = getNearestIndex(x)
      if (i < 0 || i > tetrahedra.length) println(s"x = $x, i = $i, cumulative.length = ${cumulative.length}")
      tetrahedra(i).random(r)
    }

  }
}