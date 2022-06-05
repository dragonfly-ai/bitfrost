package ai.dragonfly.bitfrost.cie

import ai.dragonfly.bitfrost.color.spectral.SampleSet
import ai.dragonfly.bitfrost.visualization.*
import ai.dragonfly.math.matrix.PCA
import ai.dragonfly.math.matrix.data.StaticUnsupervisedData
import ai.dragonfly.math.squareInPlace
import ai.dragonfly.math.stats.geometry.Tetrahedron
import ai.dragonfly.math.stats.probability.distributions.Sampleable
import ai.dragonfly.math.stats.probability.distributions.stream.StreamingVectorStats
import ai.dragonfly.math.vector.{VECTORS, Vector3}

import java.io.PrintWriter
import scala.collection.immutable
import scala.collection.mutable

trait Gamut { self: WorkingSpace =>
  object Gamut {

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

    def fromRGB(n: Int = 64, transform: XYZ => Vector3 = (v: XYZ) => Vector3(v.values)): Gamut = {

      val mesh: VolumeMesh = VolumeMesh.cube(1.0, n)

      new Gamut(
        VolumeMesh(
          mesh.vertices.map((vRGB:Vector3) => transform(RGB(vRGB.values).toXYZ)),
          mesh.triangles
        )
      )
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

      val points: Array[Vector3] = spectralSamples.volumePoints.map( transform )

      val triangles:mutable.HashSet[IndexTriangle] = mutable.HashSet[IndexTriangle]()

      var t:Int = 0
      def addTriangle(pi0:Int, pi1:Int, pi2:Int): Unit = {
        if (VolumeMesh.nonZeroArea(points(pi0), points(pi1), points(pi2))) {
          triangles += IndexTriangle(pi2, pi1, pi0)
        }
        t += 1
      }

      //addTriangle(Tetrahedron(mean, points(0), points(1), points(spectralSamples.sampleCount)))
      addTriangle(0, 1, spectralSamples.sampleCount)

      // black adjacent:
      while (t < spectralSamples.sampleCount) addTriangle(0, t + 1, t)

      val hEnd: Int = points.length - spectralSamples.sampleCount

      val end = (2 * (points.length - 1)) - spectralSamples.sampleCount
      while (t < end) {
        val i: Int = (t - spectralSamples.sampleCount) / 2
        if (i < hEnd) {
          val h: Int = i + spectralSamples.sampleCount
          if (t % 2 == 1) addTriangle(i, h, h - 1) // Tetrahedron(mean, points(i), points(h), points(h - 1))
          else addTriangle(i+1, h, i) // Tetrahedron(mean, points(i + 1), points(h), points(i))
        } else {
          val h: Int = points.length - 1
          addTriangle(i, h, h - 1) // Tetrahedron(mean, points(i), points(h), points(h - 1))
        }
      }

      // white adjacent:
      for (i <- (points.length - 1) - spectralSamples.sampleCount until points.length - 2) addTriangle(i, i + 1, points.length - 1)

      new Gamut(VolumeMesh(points, triangles))
    }


//    println("defined Gamut object methods")
  }


  /**
   *
   * @param tetrahedra
   * @param cumulative
   */

  case class Gamut (volumeMesh:VolumeMesh) extends Sampleable[Vector3] {

    val mean: Vector3 = {
      val sv2 = new StreamingVectorStats(3)
      volumeMesh.vertices.foreach((p:Vector3) => sv2(p))
      Vector3(sv2.average().values)
    }

    val maxDistSquared: Double = Gamut.computeMaxDistSquared(volumeMesh.vertices, mean)

    val tetrahedra: Array[Tetrahedron] = Array.tabulate[Tetrahedron](volumeMesh.triangles.length)((i:Int) => {
      val t: IndexTriangle = volumeMesh.triangles(i)
      Tetrahedron(
        mean,
        volumeMesh.vertices(t.v0),
        volumeMesh.vertices(t.v1),
        volumeMesh.vertices(t.v2)
      )
    })

    val cumulative: Array[Double] = {
      var totalVolume: Double = 0.0
      val ca:Array[Double] = Array.tabulate[Double](volumeMesh.triangles.length)((i:Int) => {
        totalVolume += tetrahedra(i).volume
        totalVolume
      })
      for (i <- ca.indices) ca(i) = ca(i) / totalVolume
      ca
    }

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