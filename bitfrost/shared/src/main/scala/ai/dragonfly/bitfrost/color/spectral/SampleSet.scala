package ai.dragonfly.bitfrost.color.spectral

import ai.dragonfly.math.squareInPlace
import ai.dragonfly.math.vector.Vector3
import narr.*

trait SampleSet {

  def samples: NArray[Sample]

  def sampleCount: Int = samples.length

  lazy val volumePoints:NArray[Vector3] = {

    //val points: NArray[Vector3] = new NArray[Vector3](squareInPlace(samples.length))
    val points: NArray[Vector3] = new NArray[Vector3](2 + (samples.length * (samples.length - 1)))

    points(0) = Vector3(0.0, 0.0, 0.0)

    val xyzʷ:Vector3 = {
      val v: Vector3 = Vector3(0.0, 0.0, 0.0)
      for (s <- samples) v.add(s.xyz)
      v
    }

    var p: Int = 1

    points(points.length - 1) = Vector3(1.0, 1.0, 1.0)

    for (i <- 0 until samples.length - 1) {
      for (j <- 0 until samples.length) {
        val v: Vector3 = Vector3(0.0, 0.0, 0.0)
        for (k <- 0 to i) {
          v.add(samples((j + k) % samples.length).xyz)
        }
        points(p) = Vector3(v.x / xyzʷ.x, v.y / xyzʷ.y, v.z / xyzʷ.z)
        p += 1
      }
    }

//    println(s"${points.length} vs $p")
//
//    // normalize:
//    for (i <- points.indices) {
//      val xyz: Vector3 = points(i)
//      points(i) =  Vector3(
//        xyz.x / sv.maxValues(0),
//        xyz.y / sv.maxValues(1),
//        xyz.z / sv.maxValues(2)
//      )
//    }

    points
  }
}