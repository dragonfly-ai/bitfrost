
import ai.dragonfly.bitfrost.*
import ai.dragonfly.bitfrost.cie.WorkingSpace
import ai.dragonfly.bitfrost.ColorContext.sRGB
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

    val commonSpaces: Seq[Space[_]] = Seq[Space[_]](RGB, ARGB32, ARGB64, RGBA32, RGBA64, CMYK, HSL, HSV, XYZ, Lab, Luv)

    for (space <- commonSpaces) {
      type C = space.COLOR
      // generate random color palette
      val colors: mutable.HashSet[C] = mutable.HashSet[C]()
      while (colors.size < 20) {
        val c: C = space.random().asInstanceOf[C]
        if (!colors.contains(c)) colors.add(c)
      }

      val cp = ColorPalette[C](immutable.HashMap.from[C, Int](colors.map {_ -> r.nextInt(1000)}))

      val content = div(
        div(
          s"Random Color Palette from $space space:",
          br,
          table(
            tr(td("Color"), td("Frequency")),
            cp.colorFrequencies.map {
              cf => tr(td(backgroundColor := ARGB32.fromRGB(cf.color.toRGB.asInstanceOf[sRGB.RGB]).html())(raw("&nbsp;")), td(f"${cf.frequency}%.3f%%"))
            }
          )
        ),
        div(
          "Generate random colors and search palette for nearest match:",
          br,
          table(
            tr(td("Random Color"), td("Nearest Match from Palette"), td(s"Distance in $space Space")),
            (1 to 10).map(_ => {
              val qc: C = space.random().asInstanceOf[C]
              val m: C = cp.nearestMatch(qc).color

              tr(
                td(backgroundColor := ARGB32.fromRGB(qc.toRGB.asInstanceOf[sRGB.RGB]).html())(raw("&nbsp;")),
                td(backgroundColor := ARGB32.fromRGB(m.toRGB.asInstanceOf[sRGB.RGB]).html())(raw("&nbsp;")),
                td(f"${qc.similarity(m)}%.3f%%")
              )
            })
          )
        )
      )
      println(content)
    }
  }
}
