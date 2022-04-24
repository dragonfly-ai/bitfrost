
import ai.dragonfly.bitfrost.*
import ai.dragonfly.bitfrost.colorspace.WorkingSpace
import ai.dragonfly.bitfrost.context.Adobe_RGB_1998
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
    val colors:mutable.HashSet[NRGB] = mutable.HashSet[NRGB]()
    while (colors.size < 50) {
      val c:NRGB = NRGB.random()
      if (!colors.contains(c)) colors.add(c)
    }

    val cp = ColorPalette[NRGB]( immutable.HashMap.from[NRGB, Int](colors.map { _ -> r.nextInt(1000) }) )

    val content = div(
      div(
        "Random Color Palette:",
        br,
        table(
          tr(td("Color"), td("Frequency")),
          cp.colorFrequencies.map {
            cf => tr(td(backgroundColor := cf.color.toARGB.html())(raw("&nbsp;")), td(cf.frequency))
          }
        )
      ),
      div(
        "Generate random colors and search palette for nearest match:",
        br,
        table(
          tr(td("Random Color"), td("Nearest Match from Palette"), td("Distance in L*a*b* Space")),
          (1 to 100).map( _ => {
            val nrgb: NRGB = NRGB.random()
            val m: NRGB = cp.nearestMatch(nrgb).color
            tr( td(backgroundColor := nrgb.toARGB.html())(raw("&nbsp;")), td(backgroundColor := m.toARGB.html())(raw("&nbsp;")), td(nrgb.similarity(m)))
          })
        )
      )
    )
    println(content)
  }
}
