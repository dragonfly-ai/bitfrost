package ai.dragonfly.bitfrost.visualization

import narr.*
import ai.dragonfly.math.vector.{Vector3, VectorBounds}

// maybe this should be path dependent?
case class IndexTriangle(v0:Int, v1:Int, v2:Int) {
  def area(vertices:NArray[Vector3]):Double = {
    0.5 * (vertices(v1) - vertices(v0)).cross(vertices(v2) - vertices(v0)).magnitude
  }
  def bounds(vertices:NArray[Vector3]):VectorBounds[Vector3] = VectorBounds[Vector3]( // min: V, MAX: V
    Vector3(
      Math.min(vertices(v0).x, Math.min(vertices(v1).x, vertices(v2).x) ),
      Math.min(vertices(v0).y, Math.min(vertices(v1).y, vertices(v2).y) ),
      Math.min(vertices(v0).z, Math.min(vertices(v1).z, vertices(v2).z) )
    ),
    Vector3(
      Math.max(vertices(v0).x, Math.max(vertices(v1).x, vertices(v2).x)),
      Math.max(vertices(v0).y, Math.max(vertices(v1).y, vertices(v2).y)),
      Math.max(vertices(v0).z, Math.max(vertices(v1).z, vertices(v2).z))
    )
  )
}
