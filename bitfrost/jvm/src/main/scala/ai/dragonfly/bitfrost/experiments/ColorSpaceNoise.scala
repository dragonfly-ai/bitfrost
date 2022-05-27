package ai.dragonfly.bitfrost.experiments

import ai.dragonfly.bitfrost.ColorContext
import ai.dragonfly.bitfrost.cie.*
import ai.dragonfly.bitfrost.color.model.*
import ai.dragonfly.bitfrost.color.spectral.{DEFAULT, SampleSet}
import ai.dragonfly.math.vector.Vector3

import java.awt.image.BufferedImage
import java.io.{File, FileOutputStream}
import javax.imageio.ImageIO

import scala.language.implicitConversions

object ColorSpaceNoise extends App {

  val (w: Int, h: Int) = (512, 512)
  val bi = new BufferedImage(w, h, BufferedImage.TYPE_INT_ARGB)

  val contexts = ai.dragonfly.bitfrost.ColorContext.knownContexts

  for (context <- contexts) {
    import context.*

    def noisyImage(space:Space[_], transform: Vector3 => ColorContext.sRGB.ARGB32):Unit = {

      for (y <- 0 until h) {
        for (x <- 0 until w) {
          bi.setRGB(x, y, transform(Vector3(space.random().asInstanceOf[Model[_]].toXYZ.values)).argb)
        }
      }

      println(s"Writing ./demo/image/$context$space.png")
      ImageIO.write(bi, "PNG", new File(s"./demo/image/$context$space.png"))
    }

    val XYZtoARGB32:Vector3 => ColorContext.sRGB.ARGB32 = {
      import ColorContext.sRGB
      if (context == sRGB.ARGB32) {
        (v: Vector3) => sRGB.ARGB32.fromXYZ(sRGB.XYZ(v.values)).asInstanceOf[sRGB.ARGB32]
      } else {
        val chromaticAdapter: ChromaticAdaptation[context.type, sRGB.type] = ChromaticAdaptation[context.type, sRGB.type](context, sRGB)
        (v: Vector3) => {
          sRGB.ARGB32.fromXYZ(chromaticAdapter(XYZ(v.values)))
        }
      }
    }

    Gamut.writePLY(
      Gamut.fromSpectralSamples(DEFAULT, context.illuminant),
      XYZtoARGB32,
      new java.io.FileOutputStream( new File(s"./demo/ply/${context}XYZ.ply") )
    )

    Gamut.writePLY(
      Gamut.fromRGB(),
      XYZtoARGB32,
      new java.io.FileOutputStream( new File(s"./demo/ply/${context}RGB->XYZ.ply") )
    )

    val commonSpaces: Seq[Space[_]] = Seq[Space[_]](ARGB32, CMYK, HSL, HSV)
    for (space <- commonSpaces) {
      noisyImage(space, XYZtoARGB32)
    }

    val perceptualSpaces: Seq[PerceptualSpace[_]] = Seq[PerceptualSpace[_]](XYZ, Lab, Luv)

    for (space <- perceptualSpaces) {

      val spaceToARGB32:Vector3 => ColorContext.sRGB.ARGB32 = (v:Vector3) => XYZtoARGB32(Vector3(space(v.values).toXYZ.values))

      val os:java.io.OutputStream = new java.io.FileOutputStream( new File(s"./demo/ply/$context$space.ply") )
      Gamut.writePLY(
        space.fullGamut,
        spaceToARGB32,
        os
      )

      val rgbGamut:Gamut = Gamut.fromRGB(transform = (v:XYZ) => {
        val c:PerceptualModel[_] = space.fromXYZ(v).asInstanceOf[PerceptualModel[_]]
        Vector3(c.values)
      })
      Gamut.writePLY(
        rgbGamut,
        spaceToARGB32,
        new java.io.FileOutputStream( new File(s"./demo/ply/${context}_${space}fromRGB.ply") )
      )

      noisyImage(space, XYZtoARGB32)

    }


  }

}
