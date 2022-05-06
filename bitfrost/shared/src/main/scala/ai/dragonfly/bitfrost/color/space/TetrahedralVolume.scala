package ai.dragonfly.bitfrost.color.space

import ai.dragonfly.math.stats.geometry.Tetrahedron
import ai.dragonfly.math.stats.probability.distributions.Sampleable
import ai.dragonfly.math.stats.probability.distributions.stream.StreamingVectorStats
import ai.dragonfly.math.vector.Vector3

import scala.collection.immutable


object TetrahedralVolume {

  def apply(cumulativelyWeightedTrahedra: immutable.Seq[(Double, Tetrahedron)]):TetrahedralVolume = {
    val totalVolume:Double = cumulativelyWeightedTrahedra.last._1
//    println(s"TetrahedralVolume.apply(...): totalVolume = $totalVolume")
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
    TetrahedralVolume(tetrahedra, cumulative)
  }

}

case class TetrahedralVolume private (tetrahedra:Array[Tetrahedron], cumulative:Array[Double]) extends Sampleable[Vector3] {

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
    tetrahedra(i).random()
  }

}
