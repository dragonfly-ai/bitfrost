package ai.dragonfly.bitfrost.experiments

import ai.dragonfly.bitfrost.cie.*
import ai.dragonfly.bitfrost.color.space.*
import ai.dragonfly.bitfrost.color.model.*
import ai.dragonfly.bitfrost.color.model.perceptual.XYZ
import ai.dragonfly.bitfrost.color.spectrum.SpectralTables
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
    import context.*

    ai.dragonfly.bitfrost.color.visualizaton.IO.writeMesh(context)(
      Gamut.fromSpectralSamples(
        SpectralTables.XYZ_5NM_WITH_0_1NM_PEAKS_CIE2006,
        (v:Vector3) => Vector3(
          context.illuminant.vector.x * v.x,
          context.illuminant.vector.y * v.y,
          context.illuminant.vector.z * v.z
        )
      ),
      (v: Vector3) => { context.ARGB32.fromRGB(XYZ.toRGB(context)(v)) },
      new java.io.FileOutputStream( new File(s"./demo/ply/${context}XYZ.ply") )
    )

    ai.dragonfly.bitfrost.color.visualizaton.IO.writeMesh(context)(
      Gamut.fromRGB(context)(),
      (v: Vector3) => { //println(v);
        context.ARGB32.fromRGB(XYZ.toRGB(context)(v)) },
      new java.io.FileOutputStream( new File(s"./demo/ply/${context}RGB->XYZ.ply") )
    )

//    val colorSpaces: Seq[ColorSpace[_, _]] = Seq[ColorSpace[_, _]](
//      ARGB32, RGB, CMYK, HSL, HSV, Lab, Luv
//    )

    val colorSpaces: Seq[PerceptualColorSpace[_]] = Seq[PerceptualColorSpace[_]](Lab, Luv)

    for (space <- colorSpaces) {
      val os:java.io.OutputStream = new java.io.FileOutputStream( new File(s"./demo/ply/$context$space.ply") )
      IO.writeMesh(context)(
        space.fullGamut,
        (v2c: Vector3) => {
          val xyz = space(v2c.x, v2c.y, v2c.z).toXYZ
          context.ARGB32.fromRGB(XYZ.toRGB(context)(xyz))
        },
        os
      )

      val rgbGamut:Gamut = Gamut.fromRGB(context)(transform = (v:XYZ) => Vector3(space.fromXYZ(v).values))
      ai.dragonfly.bitfrost.color.visualizaton.IO.writeMesh(context)(
        rgbGamut,
        (v: Vector3) => { context.ARGB32.fromRGB(XYZ.toRGB(context)(space(v.x, v.y, v.z).toXYZ)) },
        new java.io.FileOutputStream( new File(s"./demo/ply/${context}_${space}fromRGB.ply") )
      )

      for (y <- 0 until h) {
        for (x <- 0 until w) {
          val rcv = rgbGamut.random()
          val pixel: Int = space(rcv.x, rcv.y, rcv.z) match {
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
