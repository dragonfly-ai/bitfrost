package ai.dragonfly.bitfrost.cie

import narr.{NArray, *}
import ai.dragonfly.bitfrost.color.spectral.SampleSet
import ai.dragonfly.bitfrost.visualization.*

import ai.dragonfly.math.matrix.ml.unsupervised.dimreduction.PCA
import ai.dragonfly.math.matrix.ml.data.*
import ai.dragonfly.math.squareInPlace
import ai.dragonfly.math.stats.geometry.Tetrahedron
import ai.dragonfly.math.stats.probability.distributions.Sampleable
import ai.dragonfly.math.stats.probability.distributions.stream.{Gaussian, StreamingVectorStats}
import ai.dragonfly.math.vector.*
import ai.dragonfly.mesh.*
import ai.dragonfly.mesh.shape.*

import java.io.PrintWriter
import scala.collection.immutable
import scala.collection.mutable

trait Gamut { self: WorkingSpace =>
  object Gamut {

    def computeMaxDistSquared(points: NArray[Vector3], mean: Vector3): Double = {

      val vs: NArray[Vector] = new NArray[Vector](points.length)
      var i:Int = 0; while (i < points.length) {
        vs(i) = points(i) - mean
        i += 1
      }

      val pca = PCA(new StaticUnsupervisedData(vs))

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

    def fromRGB(n: Int = 32, transform: XYZ => Vector3 = (v: XYZ) => Vector3(v.values)): Gamut = {

      val m1: Mesh = Cube(1.0, n)

      val m2: Mesh = Mesh(
        NArray.tabulate[Vector3](m1.points.length)((i:Int) => {m1.points(i) * 255.0}),
        m1.triangles
      )

      val m3: Mesh = Mesh(
        m1.points.map((vRGB:Vector3) => transform(RGB(vRGB.values).toXYZ)),
        m1.triangles
      )

      val sg:Gaussian = Gaussian()

      var i:Int = 0; while (i < m1.triangles.length) {
        val t:Triangle = m1.triangles(i)
        sg.observe( Math.sqrt( t.area(m3.points) / t.area(m2.points) ) )
        i += 1
      }

      println(s"$ctx triangle stretch stats: ${sg.estimate}")

      new Gamut(
        m3
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

      val points: NArray[Vector3] = NArray.tabulate[Vector3](spectralSamples.volumePoints.length)(
        (i:Int)=> transform(spectralSamples.volumePoints(i))
      )

      val triangles:mutable.HashSet[Triangle] = mutable.HashSet[Triangle]()

      var t:Int = 0
      def addTriangle(pi0:Int, pi1:Int, pi2:Int): Unit = {
        if (Triangle.nonZeroArea(points(pi0), points(pi1), points(pi2))) {
          triangles += Triangle(pi2, pi1, pi0)
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

      new Gamut(Mesh.fromPointsAndHashSet(points, triangles, "Spectral Samples Gamut"))
    }

//    println("defined Gamut object methods")
  }


  /**
   *
   * @param tetrahedra
   * @param cumulative
   */

  case class Gamut (volumeMesh:Mesh) extends Sampleable[Vector3] {

    val mean: Vector3 = {
      val sv2:StreamingVectorStats[Vector3] = new StreamingVectorStats[Vector3](3)
      volumeMesh.points.foreach((p:Vector3) => sv2(p))
      Vector3(sv2.average().values)
    }

    val maxDistSquared: Double = Gamut.computeMaxDistSquared(volumeMesh.points, mean)

    val tetrahedra: NArray[Tetrahedron] = NArray.tabulate[Tetrahedron](volumeMesh.triangles.length)((i:Int) => {
      val t: Triangle = volumeMesh.triangles(i)
      Tetrahedron(
        mean,
        volumeMesh.points(t.v1),
        volumeMesh.points(t.v2),
        volumeMesh.points(t.v3)
      )
    })

    val cumulative: NArray[Double] = {
      var totalVolume: Double = 0.0
      val ca:NArray[Double] = NArray.tabulate[Double](volumeMesh.triangles.length)((i:Int) => {
        totalVolume += tetrahedra(i).volume
        totalVolume
      })
      var i:Int = 0; while (i < ca.length) {
        ca(i) = ca(i) / totalVolume
        i += 1
      }
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