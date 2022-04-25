package ai.dragonfly.bitfrost.experiments

import ai.dragonfly.bitfrost.color.ColorModelCompanion
import ai.dragonfly.bitfrost.context.Adobe_RGB_1998

import java.awt.image.BufferedImage
import java.io.File
import javax.imageio.ImageIO

object ColorSpaceNoise extends App {
  val (w: Int, h: Int) = (512, 512)
  val bi = new BufferedImage(w, h, BufferedImage.TYPE_INT_ARGB)

  import Adobe_RGB_1998.*

  println(ARGB32(50, 100, 150))
  println(ARGB32.random())

  val colorSpaces: Seq[ColorModelCompanion[_]] = Seq[ColorModelCompanion[_]](
    ARGB32, RGB, CMYK, HSL, HSV
  )

  for (space <- colorSpaces) {
    for (y <- 0 until h) {
      for (x <- 0 until w) {
        val pixel: Int = space.random() match {
          case argb32: ARGB32 => argb32.argb
          case rgb: RGB => ARGB32.fromRGB(rgb).argb
          case c: CommonColor[_] => ARGB32.fromRGB(c.toRGB).argb
        }
        bi.setRGB(x, y, pixel)
      }
    }
    ImageIO.write(bi, "PNG", new File(s"./demo/image/$space.png"))
  }
}
