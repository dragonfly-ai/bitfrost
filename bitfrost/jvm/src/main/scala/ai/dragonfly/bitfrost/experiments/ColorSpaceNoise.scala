package ai.dragonfly.bitfrost.experiments

import ai.dragonfly.bitfrost.cie.*
import ai.dragonfly.bitfrost.color.space.*
import ai.dragonfly.bitfrost.color.model.*
import ai.dragonfly.math.vector.Vector3
import ai.dragonfly.bitfrost.color.visualizaton.*


import java.awt.image.BufferedImage
import java.io.{File, FileOutputStream}
import javax.imageio.ImageIO

object ColorSpaceNoise extends App {
  val (w: Int, h: Int) = (512, 512)
  val bi = new BufferedImage(w, h, BufferedImage.TYPE_INT_ARGB)

  val contexts = ai.dragonfly.bitfrost.ColorContext.knownContexts
  for (context <- contexts) {
    //val context = ai.dragonfly.bitfrost.ColorContext.sRGB
    import context.*

    ai.dragonfly.bitfrost.color.visualizaton.IO.writeMesh(context)(
      XYZ.tetrahedralVolume(GamutXYZ.CuratedCIE2006_5nm),
      (v: Vector3) => { context.ARGB32.fromRGB(XYZ.toRGB(context)(v)) },
      new java.io.FileOutputStream( new File(s"./demo/ply/${context}XYZ.ply") )
    )
//
//    println(ARGB32(50, 100, 150))
//    println(ARGB32.random())
//
////    val colorSpaces: Seq[ColorSpace[_, _]] = Seq[ColorSpace[_, _]](
////      ARGB32, RGB, CMYK, HSL, HSV, Lab, Luv
////    )

    val colorSpaces: Seq[PerceptualColorSpace[_, _]] = Seq[PerceptualColorSpace[_, _]](Lab, Luv)

    for (space <- colorSpaces) {
      val os:java.io.OutputStream = new java.io.FileOutputStream( new File(s"./demo/ply/$context$space.ply") )
      IO.writeMesh(context)(
        space.tetrahedralVolume,
        (v2c: Vector3) => {
          context.ARGB32.fromRGB(XYZ.toRGB(context)(space(v2c.x, v2c.y, v2c.z).toXYZ))
        },
        os )

      for (y <- 0 until h) {
        for (x <- 0 until w) {
          val pixel: Int = space.random() match {
            case argb32: ARGB32 => argb32.argb
            case rgb: RGB => ARGB32.fromRGB(rgb).argb
            case c: ColorModel[_] => ARGB32.fromRGB(c.toRGB.asInstanceOf[RGB]).argb
          }
          bi.setRGB(x, y, pixel)
        }
      }
      println(s"Writing ./demo/image/$space.png")
      ImageIO.write(bi, "PNG", new File(s"./demo/image/$context$space.png"))
    }
  }
}
