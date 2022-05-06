package ai.dragonfly.bitfrost.color.model

import ai.dragonfly.bitfrost.cie.*
import ai.dragonfly.math.vector.*


/**
 * Color is the base trait from which all other color types inherit.
 */

trait ColorModel[C <: ColorModel[C]] {
  def similarity(that:C):Double
  def toRGB:rgb.RGB#RGB
}

trait DiscreteColorModel[C <: DiscreteColorModel[C]] extends ColorModel[C] {
}

trait CylindricalColorModel[C <: CylindricalColorModel[C]] extends ColorModel[C] {
  val values:VectorValues
}

trait VectorColorModel[C <: VectorColorModel[C]] extends ColorModel[C] with Vector {
  override def similarity(that: C): Double = this.euclid.distanceTo(that)
}

trait PerceptualColorModel[C <: PerceptualColorModel[C]] extends VectorColorModel[C] {
  def toXYZ: XYZ
  
}
