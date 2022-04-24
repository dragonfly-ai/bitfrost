package ai.dragonfly.bitfrost

import ai.dragonfly.math.vector.Vector3

package object colorspace {

  type XYZ = Vector3

  // constants
  // http://www.brucelindbloom.com/index.html?LContinuity.html

  val ϵ:Double = 0.008856451679035631
  val `ϵ³`:Double = 6.946711652838615E-7
  val `∛ϵ`:Double = 0.20689655172413793

  val k:Double = 903.2962962962963
  val kϵ: Double = 8.0


  val `k/116`:Double = 7.787037037037037

  val `116/k`:Double = 0.12841854934601665
  val `16/k`:Double = 0.017712903358071262
  val `1/k`:Double = 0.0011070564598794539

  val `16/116`:Double = 0.13793103448275862
  val `1/116`:Double = 0.008620689655172414
  val `1/116³`:Double = 6.406576735413506E-7

  val Yn: Double = 100.0
  val `1/Yn`: Double = 0.01
}
