
import ai.dragonfly.bitfrost.*
import ai.dragonfly.bitfrost.cie.WorkingSpace
import ai.dragonfly.bitfrost.ColorContext.Adobe_RGB_1998
import ai.dragonfly.math.Random.defaultRandom

import scala.collection.mutable
import scala.util.Random
import palette.immutable.ColorPalette

import scala.collection.immutable
import scalatags.Text.all.*

object Demo {

  val r:Random = defaultRandom

  def main(args: Array[String]): Unit = {

    import Adobe_RGB_1998.*

    // generate random color palette
    val colors:mutable.HashSet[RGB] = mutable.HashSet[RGB]()
    while (colors.size < 50) {
      val c:RGB = RGB.random()
      if (!colors.contains(c)) colors.add(c)
    }

    val cp = ColorPalette[RGB]( immutable.HashMap.from[RGB, Int](colors.map { _ -> r.nextInt(1000) }) )

    val content = div(
      div(
        "Random Color Palette:",
        br,
        table(
          tr(td("Color"), td("Frequency")),
          cp.colorFrequencies.map {
            cf => tr(td(backgroundColor := ARGB32.fromRGB(cf.color).html())(raw("&nbsp;")), td(cf.frequency))
          }
        )
      ),
      div(
        "Generate random colors and search palette for nearest match:",
        br,
        table(
          tr(td("Random Color"), td("Nearest Match from Palette"), td("Distance in L*a*b* Space")),
          (1 to 100).map( _ => {
            val rgb: RGB = RGB.random()
            val m: RGB = cp.nearestMatch(rgb).color
            tr( td(backgroundColor := ARGB32.fromRGB(rgb).html())(raw("&nbsp;")), td(backgroundColor := ARGB32.fromRGB(m).html())(raw("&nbsp;")), td(rgb.similarity(m)))
          })
        )
      )
    )
    println(content)
  }
}
