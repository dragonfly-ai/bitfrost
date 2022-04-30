package ai.dragonfly.bitfrost

package object color {


  import ai.dragonfly.bitfrost.*
  import ai.dragonfly.bitfrost.cie.{WorkingSpace, XYZ}
  import ai.dragonfly.bitfrost.color.model.*
  import ai.dragonfly.math.stats.probability.distributions.Sampleable
  import ai.dragonfly.math.vector.*

  /**
   * Color is the base trait from which all other color types inherit.
   */

  trait Color[C <: Color[C]] {
    def similarity(that:C):Double
  }

  trait DiscreteColor[C <: DiscreteColor[C]] extends Color[C] {
  }

  trait VectorColor[C <: VectorColor[C]] extends Color[C] with Vector {
    override def similarity(that: C): Double = this.euclid.distanceTo(that)
  }

  trait PerceptualColor[C <: PerceptualColor[C]] extends VectorColor[C] {
    def toXYZ: XYZ
  }

  trait PerceptualColorCompanion[C <: PerceptualColor[C]] extends VectorColorModelCompanion[C] {
    def fromXYZ(xyz: XYZ): C

    import ai.dragonfly.math.stats.geometry.Tetrahedron
    import ai.dragonfly.math.stats.probability.distributions.Sampleable
    import ai.dragonfly.math.vector.Vector3

    class TetrahedralVolume(center:Vector3) {

      // Build a tetrahedron from each triangle.
      // Build a histogram out of all of the tetrahedra.

      var totalVolume = 0.0
      var count: Int = 0
      val tetrahedrons = new Array[Tetrahedron](768108)
      val cumulative = new Array[Double](768108)

      def addTetrahedrons(p0: Vector3, p1: Vector3, p2: Vector3, p3: Vector3): Unit = {
        var t = Tetrahedron(p0, p1, p2, center)

        totalVolume = totalVolume + t.volume
        cumulative(count) = totalVolume
        tetrahedrons(count) = t
        count = count + 1

        t = Tetrahedron(p3, p2, p1, center)
        totalVolume = totalVolume + t.volume
        tetrahedrons(count) = t
        cumulative(count) = totalVolume

        count = count + 1
      }

      // generate tetrahedra
      var c:Double = 0.0
      var i:Double = 0.0
      while (c < 1.0){
        i = 0.0
        while (i < 1.0) {
          addTetrahedrons(
            Vector3(1.0, c, i),
            Vector3(1.0, c + 1.0, i),
            Vector3(1.0, c, i + 1.0),
            Vector3(1.0, c + 1.0, i + 1.0)
          )

          addTetrahedrons(
            Vector3(0, c, i),
            Vector3(0, c + 1.0, i),
            Vector3(0, c, i + 1.0),
            Vector3(0, c + 1.0, i + 1.0)
          )

          addTetrahedrons(
            Vector3(c, 1.0, i),
            Vector3(c + 1.0, 1.0, i),
            Vector3(c, 1.0, i + 1.0),
            Vector3(c + 1.0, 1.0, i + 1.0)
          )

          addTetrahedrons(
            Vector3(c, 0, i),
            Vector3(c + 1.0, 0, i),
            Vector3(c, 0, i + 1.0),
            Vector3(c + 1.0, 0, i + 1.0)
          )

          addTetrahedrons(
            Vector3(c, i, 1.0),
            Vector3(c + 1.0, i, 1.0),
            Vector3(c, i + 1.0, 1.0),
            Vector3(c + 1.0, i + 1.0, 1.0)
          )

          addTetrahedrons(
            Vector3(c, i, 0),
            Vector3(c + 1.0, i, 0),
            Vector3(c, i + 1.0, 0),
            Vector3(c + 1.0, i + 1.0, 0)
          )
          i = i + 0.001
        }
        c = c + 0.001
      }

      var ti: Int = 0
      var sum: Double = 0.0
      for (t <- tetrahedrons) {
        sum = sum + t.volume
        cumulative(ti) = sum
        ti = ti + 1
      }

      println(s"tetrahedrons complete: ${cumulative.length}")
      println(s"total volume: $totalVolume vs $sum")

      def getNearestIndex(target: Double): Int = {
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

      def random(): Vector3 = {
        val x = Math.random() * totalVolume
        val i = getNearestIndex(x)

        val v = tetrahedrons(i).random()
        Vector3(v.x, v.y, v.z)
      }
    }
  }


  /**
   * trait for Color Companion Objects.
   */

  trait ColorModelCompanion[C <: Color[C]] extends Sampleable[C] {
    /**
     * Computes a weighted average of two colors in C color space.
     * @param c1 the first color.
     * @param w1 the weight of the first color in the range of [0-1].
     * @param c2 the second color.
     * @param w2 the weight of the second color in the range of [0-1].
     * @return the weighted average: c1 * w1 + c2 * w2.
     */
    def weightedAverage(c1: C, w1: Double, c2: C, w2: Double): C

  }

  trait DiscreteColorModelCompanion[C <: DiscreteColor[C]] extends ColorModelCompanion[C] {

  }

  trait VectorColorModelCompanion[C <: VectorColor[C]] extends ColorModelCompanion[C] {

    /**
     * Computes a weighted average of two colors in C color space.
     * @param c1 the first color.
     * @param w1 the weight of the first color in the range of [0-1].
     * @param c2 the second color.
     * @param w2 the weight of the second color in the range of [0-1].
     * @return the weighted average: c1 * w1 + c2 * w2.
     */
    def weightedAverage(c1: C, w1: Double, c2: C, w2: Double): C = ((c1 * w1) + (c2 * w2)).asInstanceOf[C]

  }


}
