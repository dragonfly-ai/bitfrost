package ai.dragonfly.bitfrost.experiments

import ai.dragonfly.bitfrost.ColorSpace
import ai.dragonfly.bitfrost.context.Adobe_RGB_1998

import java.awt.image.BufferedImage
import java.io.File
import javax.imageio.ImageIO

object ColorSpaceNoise extends App {
  val (w: Int, h: Int) = (1080, 1080)
  val bi = new BufferedImage(w, h, BufferedImage.TYPE_INT_ARGB)

  import Adobe_RGB_1998.*

  println(ARGB(50, 100, 150))
  println(ARGB.random())

  val colorSpaces: Seq[ColorSpace[_]] = Seq[ColorSpace[_]](
    ARGB, NRGB, CMYK, HSL, HSV
  )

  for (space <- colorSpaces) {
    for (y <- 0 until h) {
      for (x <- 0 until w) {
        val pixel: Int = space.random() match {
          case c: ARGB => c.argb
          case c: NRGB => c.toARGB.argb
          case c: CommonColor[_] => c.toNRGB.toARGB.argb
        }
        bi.setRGB(x, y, pixel)
      }
    }
    ImageIO.write(bi, "PNG", new File(s"./demo/image/$space.png"))
  }
}
