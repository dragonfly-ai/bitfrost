
import ai.dragonfly.bitfrost.*
import ai.dragonfly.bitfrost.cie.WorkingSpace
import ai.dragonfly.bitfrost.ColorContext.sRGB
import ai.dragonfly.bitfrost.color.space.XYZ
import ai.dragonfly.math.Random.defaultRandom

import scala.collection.mutable
import scala.util.Random
import palette.immutable.ColorPalette

import scala.collection.immutable
import scalatags.Text.all.*

object Demo {

  val r:Random = defaultRandom

  def main(args: Array[String]): Unit = {

    import sRGB.*

    // generate random color palette
    val colors:mutable.HashSet[Lab] = mutable.HashSet[Lab]()
    while (colors.size < 1) {
      val c:Lab = Lab.random()
      if (!colors.contains(c)) colors.add(c)
    }

    val cp = ColorPalette[Lab]( immutable.HashMap.from[Lab, Int](colors.map { _ -> r.nextInt(1000) }) )

    val content = div(
      div(
        "Random Color Palette:",
        br,
        table(
          tr(td("Color"), td("Frequency")),
          cp.colorFrequencies.map {
            cf => tr(td(backgroundColor := ARGB32.fromRGB(XYZ.toRGB(sRGB)(cf.color.toXYZ)).html())(raw("&nbsp;")), td(cf.frequency))
          }
        )
      ),
      div(
        "Generate random colors and search palette for nearest match:",
        br,
        table(
          tr(td("Random Color"), td("Nearest Match from Palette"), td("Distance in L*a*b* Space")),
          (1 to 100).map( _ => {
            val lab: Lab = Lab.random()
            val m: Lab = cp.nearestMatch(lab).color
            tr(
              td(backgroundColor := ARGB32.fromRGB(XYZ.toRGB(sRGB)(lab.toXYZ)).html())(raw("&nbsp;")),
              td(backgroundColor := ARGB32.fromRGB(XYZ.toRGB(sRGB)(m.toXYZ)).html())(raw("&nbsp;")),
              td(lab.similarity(m))
            )
          })
        )
      )
    )
    println(content)
  }
}
