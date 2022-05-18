package ai.dragonfly.bitfrost.color.space

import ai.dragonfly.bitfrost.cie.{Illuminant, WorkingSpace}
import ai.dragonfly.bitfrost.color.spectrum.WavelengthXYZ
import ai.dragonfly.math.squareInPlace
import ai.dragonfly.math.stats.geometry.Tetrahedron
import ai.dragonfly.math.stats.probability.distributions.Sampleable
import ai.dragonfly.math.stats.probability.distributions.stream.StreamingVectorStats
import ai.dragonfly.math.vector.Vector3

import scala.collection.immutable


object Gamut {

  def apply(cumulativelyWeightedTrahedra: immutable.Seq[(Double, Tetrahedron)]):Gamut = {

    val totalVolume:Double = cumulativelyWeightedTrahedra.last._1
    println(s"TetrahedralVolume.apply(...): totalVolume = $totalVolume")

    val cumulative:Array[Double] = new Array[Double](cumulativelyWeightedTrahedra.size)
    val tetrahedra:Array[Tetrahedron] = new Array[Tetrahedron](cumulativelyWeightedTrahedra.size)
    var remaining:immutable.Seq[(Double, Tetrahedron)] = cumulativelyWeightedTrahedra
    var i:Int = 0
    while (remaining.nonEmpty) {
      val t = remaining.head
      cumulative(i) = t._1 / totalVolume
      tetrahedra(i) = t._2
      remaining = remaining.tail
      i += 1
    }
    Gamut(tetrahedra, cumulative)

  }

  def fromRGB(workingSpace: WorkingSpace)(res:Double = 0.025, transform: Vector3 => Vector3 = (v: Vector3) => v): Gamut = {

    var c: Double = 0

    var cumulativelyWeightedTrahedra: immutable.Seq[(Double, Tetrahedron)] = immutable.Seq[(Double, Tetrahedron)]()

    val svs:StreamingVectorStats = StreamingVectorStats(3)

    val size:Int = squareInPlace(1.0/res).toInt*24

    val points:Array[Vector3] = new Array[Vector3](size)
    var p:Int = 0
    def addPoints(p0: Vector3, p1: Vector3, p2: Vector3, p3: Vector3): Unit = {
      svs(p0)(p1)(p2)(p3)

      points(p) = p0
      points(p+1) = p1
      points(p+2) = p2
      points(p+3) = p3

      p = p + 4
    }

    while (c < 1.0) {

      var i: Double = 0
      while (i < 1.0) {
        addPoints(
          transform(workingSpace.RGB(1.0, c,       i      ).toXYZ),
          transform(workingSpace.RGB(1.0, c + res, i      ).toXYZ),
          transform(workingSpace.RGB(1.0, c,       i + res).toXYZ),
          transform(workingSpace.RGB(1.0, c + res, i + res).toXYZ)
        )

        addPoints(
          transform(workingSpace.RGB(0.0, c,       i      ).toXYZ),
          transform(workingSpace.RGB(0.0, c + res, i      ).toXYZ),
          transform(workingSpace.RGB(0.0, c,       i + res).toXYZ),
          transform(workingSpace.RGB(0.0, c + res, i + res).toXYZ)
        )

        addPoints(
          transform(workingSpace.RGB(c,       1.0, i      ).toXYZ),
          transform(workingSpace.RGB(c + res, 1.0, i      ).toXYZ),
          transform(workingSpace.RGB(c,       1.0, i + res).toXYZ),
          transform(workingSpace.RGB(c + res, 1.0, i + res).toXYZ)
        )

        addPoints(
          transform(workingSpace.RGB(c,       0.0, i      ).toXYZ),
          transform(workingSpace.RGB(c + res, 0.0, i      ).toXYZ),
          transform(workingSpace.RGB(c,       0.0, i + res).toXYZ),
          transform(workingSpace.RGB(c + res, 0.0, i + res).toXYZ)
        )

        addPoints(
          transform(workingSpace.RGB(c,       i,       1.0).toXYZ),
          transform(workingSpace.RGB(c + res, i,       1.0).toXYZ),
          transform(workingSpace.RGB(c,       i + res, 1.0).toXYZ),
          transform(workingSpace.RGB(c + res, i + res, 1.0).toXYZ)
        )

        addPoints(
          transform(workingSpace.RGB(c,       i,       0.0).toXYZ),
          transform(workingSpace.RGB(c + res, i,       0.0).toXYZ),
          transform(workingSpace.RGB(c,       i + res, 0.0).toXYZ),
          transform(workingSpace.RGB(c + res, i + res, 0.0).toXYZ)
        )

        i += res
      }
      c += res
    }

    println(s"p = $p vs size = $size")
    val center:Vector3 = Vector3(svs.average().values)

    def addTetrahedron(p0: Vector3, p1: Vector3, p2: Vector3, p3: Vector3): Unit = {
      addTet(Tetrahedron(center, p0, p1, p2))
      addTet(Tetrahedron(center, p3, p2, p1))
    }

    def addTet(tet: Tetrahedron):Unit = {
      if (tet == null) println("adding a null tetrahedron")
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
        points(i+1),
        points(i+2),
        points(i+3)
      )
    }

    apply(cumulativelyWeightedTrahedra)
  }

  def fromSpectralSamples(spectralSamples: Array[WavelengthXYZ], transform: Vector3 => Vector3 = (v: Vector3) => v): Gamut = {

    val sv: StreamingVectorStats = new StreamingVectorStats(3)

    val points: Array[Vector3] = new Array[Vector3](squareInPlace(spectralSamples.length))

    var p: Int = 0

    for (i <- spectralSamples.indices) {
      for (j <- spectralSamples.indices) {
        val v: Vector3 = Vector3(0.0, 0.0, 0.0)
        for (k <- 0 to i) {
          v.add(spectralSamples((j + k) % spectralSamples.length).xyz)
        }
        points(p) = v
        sv(v)
        p += 1
      }
    }

    val sv2 = new StreamingVectorStats(3)

    for (i <- points.indices) {
      val xyz: Vector3 = points(i)
      //val nxyz = Vector3(xyz.x / sv.maxValues(0), xyz.y / sv.maxValues(1), xyz.z / sv.maxValues(2))
      val nxyz = Vector3(
        xyz.x / sv.maxValues(0),
        xyz.y / sv.maxValues(1),
        xyz.z / sv.maxValues(2)
      )
      //      print(s"xyz = $xyz => nxyz = $nxyz => transform(nxyz) = ${transform(nxyz)}")
      points(i) = transform(nxyz)
      sv2(points(i))
    }

    val mean: Vector3 = Vector3(sv2.average().values)

    println(s"mean $mean")

    println(s"point count: $p")

    val hEnd: Int = points.length - spectralSamples.length
    var cumulativelyWeightedTrahedra: immutable.Seq[(Double, Tetrahedron)] = immutable.Seq[(Double, Tetrahedron)]()
    var t: Int = 0

    addTet(Tetrahedron(mean, points(0), points(1), points(spectralSamples.length)))

    def addTet(tet: Tetrahedron): Unit = {
      if (tet == null) println("adding a null tetrahedron")
      else {
        var vol: Double = tet.volume
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

    while (t < spectralSamples.length) addTet(Tetrahedron(mean, points(0), points(t + 1), points(t)))

    val end = (2 * (points.length - 1)) - spectralSamples.length
    while (t < end) {
      val i: Int = (t - spectralSamples.length) / 2
      addTet(
        if (i < hEnd) {
          val h: Int = i + spectralSamples.length
          if (t % 2 == 1) Tetrahedron(mean, points(i), points(h), points(h - 1))
          else Tetrahedron(mean, points(i + 1), points(h), points(i))
        } else {
          val h: Int = points.length - 1
          Tetrahedron(mean, points(i), points(h), points(h - 1))
        }
      )
    }

    Gamut(cumulativelyWeightedTrahedra)
  }
}

case class Gamut private(tetrahedra:Array[Tetrahedron], cumulative:Array[Double]) extends Sampleable[Vector3] {

  private def getNearestIndex(target: Double): Int = {
    var left = 0
    var right = cumulative.length - 1
    while (left <= right) {
      val mid = (left + right) / 2
      if (cumulative(mid) < target) left = mid + 1
      else if (cumulative(mid) > target) right = mid - 1
      else return mid
    }
    right
  }

  override def random(r:scala.util.Random = ai.dragonfly.math.Random.defaultRandom): Vector3 = {
    val x = r.nextDouble()
    val i = getNearestIndex(x)
//    println(s"x = $x, i = $i, cumulative.length = ${cumulative.length}")
    tetrahedra(i).random(r)
  }

}
