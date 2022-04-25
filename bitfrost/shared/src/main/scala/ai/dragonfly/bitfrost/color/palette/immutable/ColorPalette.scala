package ai.dragonfly.bitfrost.palette.immutable

import ai.dragonfly.math.matrix.*
import ai.dragonfly.bitfrost.*
import ai.dragonfly.bitfrost.cie.WorkingSpace
import ai.dragonfly.bitfrost.color.Color
import ai.dragonfly.math.Euclidean

import scala.collection.*

object ColorPalette {

  /**
    * apply method to create a ColorPalette object from a color frequency histogram.
    * @param hist a map with color objects as Keys and Integer values as frequencies.
    * @return an instance of the ColorPalette class.
    * @example {{{ val cp = ColorPalette(histogram) }}}
    */
  def apply[C <: Color[C]](hist: Map[C, Int]): ColorPalette[C] = {
    // Normalize
    val frequencyTotal: Double = hist.values.sum
    new ColorPalette[C](
      immutable.TreeSet.from[ColorFrequency[C]](
        hist.map { (c:C, f:Int) =>
          val cf = ColorFrequency[C](c, f / frequencyTotal)
          cf
        }
      )(Ordering.by[ColorFrequency[C], Double](_.frequency)).toArray
    )
  }

}

/**
  * ColorPalette organizes a sorted array of color frequencies, ranked from highest to lowest.
  * @param colorFrequencies an array of ColorFrequency objects.
  */

class ColorPalette[C <: Color[C]](val colorFrequencies: Array[ColorFrequency[C]]) {
  /**
    * Search the palette for the closest match to a query color.
    *
    * @tparam T encodes the color space to compute the color distance in.
    * @param color a color object to query with, e.g. L*a*b*, XYZ, or RGB.
    * @return an instance of the ColorFrequency class which is nearest match to the query color.
    */

  def nearestMatch(color: C): ColorFrequency[C] = {
    var distSquared = Double.MaxValue
    var colorMatch: ColorFrequency[C] = null
    for ( m <- colorFrequencies ) {
      val dist = color.similarity(m.color)
      if (dist < distSquared) {
        distSquared = dist
        colorMatch = m
      }
    }
    colorMatch
  }

  override def toString(): String = {
    val sb = new StringBuilder(colorFrequencies.length * 30)
    sb.append("ColorPalette(")
    for (cf <- colorFrequencies) {
      sb.append( cf ).append(" ")
    }
    sb.append(")")
    sb.toString()
  }
}

/**
  * ColorFrequency couples a color object to a frequency.
  *
  * @constructor Create a new RGBA object from an Int.
  * @param color a color object.
  * @param frequency a frequency normalized between 0 and 1.  This encodes the prominence of a color relative to others in a ColorPalette.
  * @return an instance of the ColorFrequency class.
  */

case class ColorFrequency[C <: Color[C]](color: C, frequency: Double) {

//
//  /**
//    * Compares this color's frequency to that color's frequency.
//    * @param cf a map with color objects as Keys and Integer values as frequencies.
//    * @return Returns x where: x < 0 when this < that, x == 0 when this == that, x > 0 when this > that
//   */
//  override def compare(cf: ColorFrequency[C]) = {
//    if (frequency < cf.frequency ) -1
//    else if (frequency > cf.frequency) 1
//    else 0
//  }
}
