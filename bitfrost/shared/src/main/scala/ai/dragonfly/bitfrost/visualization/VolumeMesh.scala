package ai.dragonfly.bitfrost.visualization

import narr.*

import ai.dragonfly.math.vector.*
import ai.dragonfly.math.Constant.π
import ai.dragonfly.math.{cubeInPlace, squareInPlace}

import java.io.File
import scala.collection.mutable

object VolumeMesh {

  inline def nonZeroArea(p0:Vector3, p1:Vector3, p2:Vector3): Boolean = (p1 - p0).cross(p2 - p0).magnitudeSquared > 0

  def corners(l:Double): NArray[NArray[Vector3]] = NArray[NArray[Vector3]](
    NArray[Vector3]( Vector3(0, 0, 0), Vector3(l, 0, 0), Vector3(l, l, 0), Vector3(0, l, 0) ),
    NArray[Vector3]( Vector3(0, 0, l), Vector3(l, 0, l), Vector3(l, l, l), Vector3(0, l, l) )
  )

  def cubePoints(l:Double, n:Int): NArray[Vector3] = {

    val basis = corners(l)

    val panelStride:Int = n - 1

    val stride:Int = 4 * panelStride
    val capStride:Int = panelStride - 1
    val capPointCount: Int = squareInPlace( capStride )

    val pointCount:Int = cubeInPlace(n) - cubeInPlace(capStride)
    val sidePointCount = pointCount - (2 * capPointCount) //points.length - (2 * capPointCount)

    val points:NArray[Vector3] = new NArray[Vector3](pointCount) //(cubeInPlace(n) - cubeInPlace(n - 2))  // n³ - (n-2)³

    for (p <- 0 until sidePointCount) {
      val i:Int = (p % stride) / panelStride

      val bottomLeft:Vector3 = basis(0)(i)
      val bottomRight:Vector3 = basis(0)((i + 1) % 4)

      val z:Double = l * ((p / stride) / panelStride.toDouble)
      val alpha:Double = (p % panelStride) / panelStride.toDouble

      points(p) = Vector3(0.0, 0.0, z) + (bottomLeft * (1.0 - alpha)) + (bottomRight * alpha)
    }

    val Δl:Double = l / (n - 1)
    var p: Int = sidePointCount

    for ( yi <- 1 to capStride ) {
      for ( xi <- 1 to capStride ) {
        val x:Double = xi * Δl
        val y:Double = yi * Δl
        points(p) = Vector3(x, y, 0)
        points(p + capPointCount) = Vector3(x, y, l)
        p += 1
      }
    }

    points
  }


  def cube(l:Double = 1.0, n:Int = 64):VolumeMesh = {

    val points: NArray[Vector3] = cubePoints(l, n)

    val panelStride:Int = n - 1

    val stride:Int = 4 * panelStride

    val triangles: NArray[IndexTriangle] = new NArray[IndexTriangle]( 12 * squareInPlace(n - 1) ) // 12(n-1)²

    var t:Int = 0

    inline def addQuad(a:Int, b:Int, c:Int, d:Int):Unit = {
      triangles(t) = IndexTriangle(a, b, c)
      triangles(t+1) = IndexTriangle(a, d, b)
      t += 2
    }

    // [front)[right)[back)[left)
    val sidePointCount = cubeInPlace(n) - cubeInPlace(n - 2) - (2 * squareInPlace(n - 2))

    for (p <- 1 until sidePointCount - stride + 1) {

      if (p % stride == 0) addQuad(p, p - 1, p - stride, p - 1 + stride)
      else addQuad(p + stride, p - 1, p, p + stride - 1)

    }

    // caps

    val capStride:Int = panelStride - 1
    val capPointCount:Int = squareInPlace(capStride)

    for ( p <- capStride + 1 until capPointCount ) {
      val pi:Int = sidePointCount + p

      if (p % capStride != 0) {
        // (bottom)
        addQuad(pi - capStride, pi - 1, pi, pi - capStride - 1)

        // (top)
        val pj:Int = sidePointCount + p + capPointCount
        addQuad(pj - 1, pj - capStride, pj, pj - capStride - 1)
      }
    }

    // Connect Caps to Sides:

    // Bottom Corners:
    // Left Front Bottom
    addQuad(stride - 1, 1, 0, sidePointCount)

    // Front Bottom Right
    addQuad(panelStride, sidePointCount + capStride - 1, panelStride + 1, panelStride - 1)

    // Bottom Right Back
    var temp:Int = 2 * panelStride
    addQuad(temp + 1, temp - 1, sidePointCount + capPointCount - 1, temp)

    // Bottom Back Left
    temp = 3 * panelStride
    addQuad(sidePointCount + capPointCount - capStride, temp, temp - 1, temp + 1)

    // Top Corners:
    val topOffset:Int = sidePointCount + capPointCount

    // Left Front Top
    temp = sidePointCount - stride
    addQuad(temp + 1, sidePointCount - 1, temp, topOffset)

    // Front Top Right
    temp = sidePointCount - stride + panelStride
    addQuad(temp, topOffset + capStride - 1, temp - 1, temp + 1)

    // Top Right Back
    temp = sidePointCount - 2 * panelStride
    addQuad(temp - 1, temp + 1, points.length - 1, temp )

    // Top Back Left
    temp = sidePointCount - panelStride
    addQuad(temp, points.length - capStride, temp - 1, temp + 1)

    // connect sides to caps
    for (i <- 0 until capStride - 1) {
      // Bottom Front
      temp = sidePointCount + i
      addQuad(temp, i + 2, i + 1, temp + 1)

      // Bottom Right
      val ibr:Int = sidePointCount + capStride - 1 + (capStride * i)
      temp = panelStride + i + 1
      addQuad(ibr + capStride, temp, ibr, temp + 1)

      // Bottom Back
      val ibb:Int = 3 * panelStride - i - 1
      temp = sidePointCount + capPointCount - capStride + i
      addQuad(ibb, temp + 1, temp, ibb - 1)

      // Bottom Left
      val ibl:Int = stride - i - 1
      temp = sidePointCount + (i * capStride)
      addQuad(ibl - 1, temp, ibl, temp + capStride)

      // Top Front
      val itf:Int = sidePointCount - stride + 1 + i
      temp = topOffset + i
      addQuad(temp, itf + 1, temp + 1, itf)

      // Top Right
      val itr:Int = sidePointCount - stride + panelStride + i + 1
      temp = topOffset + (i + 1) * capStride
      addQuad(itr, temp + capStride - 1, temp - 1, itr + 1)

      // Top Back
      val itb:Int = sidePointCount - panelStride - 1 - i
      temp = points.length - capStride + i
      addQuad(itb, temp + 1, itb - 1, temp)

      // Top Left
      val itl:Int = sidePointCount - i - 1
      temp = topOffset + i * capStride
      addQuad(temp, itl - 1, itl, temp + capStride)

    }


    VolumeMesh(points, triangles)
  }

  def cylinder(segments:Int = 180, sideSegments:Int = 1, capSegments:Int = 1, radius: Double = 1.0, height: Double = 1.0): VolumeMesh = {
    val Δθ: Double = (2 * π) / segments
    val cuts:Int = capSegments + sideSegments

    val points: NArray[Vector3] = new NArray[Vector3](2 + ((cuts + 1) * segments))
  //  println(points.length)

    val pEnd: Int = points.length - 1

    points(0) = Vector3(0, 0, 0)
    points(pEnd) = Vector3(0, 0, height)

    var dT = 0.0
    var p = 1
    var pcount = 2;
    for (cut <- 0 to capSegments + sideSegments) {

      p = 1
      while (p <= segments) {

        val (r: Double, h: Double) = if (cut < sideSegments) {
          (radius,(cut.toDouble / sideSegments.toDouble) * height)
        } else (radius * (1.0 - ((cut - sideSegments).toDouble / capSegments)), height)

        val x: Double = r * Math.cos(dT)
        val y: Double = r * Math.sin(dT)

        val cutOffset: Int = cut * segments
        points(cutOffset + p) = Vector3(x, y, h)

        dT = p * Δθ
        p = p + 1

      }
      pcount += p - 1


    }

//    println(pcount)

    val triangles: NArray[IndexTriangle] = new NArray[IndexTriangle]((2 * (cuts + 1)) * segments)

    p = 1
    var t = 0

    val coEnd: Int = cuts * segments
    while (p <= segments) {
      val ps: Int = p % segments
      triangles(t) = IndexTriangle(0, ps + 1, p) // bottom
      triangles(t + 1) = IndexTriangle(pEnd, coEnd + p, coEnd + ps + 1) // top
      t += 2
      p += 1
    }


    for (cut <- 0 until cuts) {

      val cutOffset: Int = cut * segments

      p = 1

      while (p <= segments) {
        val ps: Int = p % segments
        val off2 = cutOffset + segments
        triangles(t) = IndexTriangle(cutOffset + p, cutOffset + ps + 1, off2 + ps + 1) // side
        triangles(t + 1) = IndexTriangle(off2 + p, cutOffset + p, off2 + ps + 1) // side
        t += 2
        p += 1
      }
    }

    VolumeMesh(points, triangles)
  }

  def apply(points:NArray[Vector3], triangleSet:mutable.HashSet[IndexTriangle]):VolumeMesh = {
    val triangles:NArray[IndexTriangle] = new NArray[IndexTriangle](triangleSet.size)
    var i:Int = 0
    for (t <- triangleSet) {
      triangles(i) = t
      i += 1
    }
    VolumeMesh(points, triangles)
  }
}

case class VolumeMesh(vertices: NArray[Vector3], triangles: NArray[IndexTriangle]) {
  def transform(f: Vector3 => Vector3):VolumeMesh = VolumeMesh( vertices.map(f), triangles)
}
