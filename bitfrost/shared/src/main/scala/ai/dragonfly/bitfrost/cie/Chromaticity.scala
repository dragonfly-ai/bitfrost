package ai.dragonfly.bitfrost.cie

import Jama.Matrix
import ai.dragonfly.math.matrix.MatrixValues
import ai.dragonfly.math.vector.{Vector2, VectorValues}

object ChromaticityPrimary {
  def inferThird(cp1: ChromaticityPrimary, cp2: ChromaticityPrimary): ChromaticityPrimary = ChromaticityPrimary(
    Vector2(
      1.0 - (cp1.x + cp2.x),
      1.0 - (cp1.y + cp2.y)
    ),
    1.0 - (cp1.Y + cp2.Y)
  )
}

/**
 * Given two primaries: the third can be inferred.
 *
 * @param v Primary x, y with x between [0.0, 1.0] and y between [0.0, 1.0]
 * @param Y weight between [0.0, 1.0] (reflected by xy relative to other primaries)
 */

case class ChromaticityPrimary(v:Vector2, Y:Double) {
  export v.{x, y}
}

/**
 * Assumes:
 * RED.x + GEEN.x + BLUE.x = 1.0
 * RED.y + GEEN.y + BLUE.y = 1.0
 * RED.Y + GEEN.Y + BLUE.Y = 1.0
 *
 * @param RED red chromatic primary
 * @param GREEN green chromatic primary
 * @param BLUE blue chromatic primary
 */

case class ChromaticityPrimaries(RED: ChromaticityPrimary, GREEN: ChromaticityPrimary, BLUE: ChromaticityPrimary) {

  // http://www.brucelindbloom.com/index.html?Eqn_RGB_XYZ_Matrix.html
  lazy val xyzXrgb_T:Matrix = new Matrix(
    MatrixValues(
      VectorValues( RED.x / RED.y, 1.0, (1.0 - RED.x - RED.y) / RED.y ),
      VectorValues( GREEN.x / GREEN.y, 1.0, (1.0 - GREEN.x - GREEN.y) / GREEN.y ),
      VectorValues( BLUE.x / BLUE.y, 1.0, (1.0 - BLUE.x - BLUE.y) / BLUE.y )
    )
  )

  lazy val xyzXrgbInv:Matrix = xyzXrgb_T.transpose().inverse()
}
