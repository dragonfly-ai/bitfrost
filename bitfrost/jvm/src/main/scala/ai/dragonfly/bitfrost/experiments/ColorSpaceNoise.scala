package ai.dragonfly.bitfrost.experiments

import ai.dragonfly.bitfrost.ColorContext
import ai.dragonfly.bitfrost.cie.*
import ai.dragonfly.bitfrost.color.model.*
import ai.dragonfly.bitfrost.color.spectral.*
import ai.dragonfly.bitfrost.visualization.*
import ai.dragonfly.mesh.sRGB
import ai.dragonfly.mesh.io.PLY
import ai.dragonfly.math.vector.Vector3

import java.awt.image.BufferedImage
import java.io.{File, FileOutputStream}
import javax.imageio.ImageIO
import scala.language.implicitConversions

object ColorSpaceNoise extends App {

  println("Starting ColorSpaceNoise")

  val (w: Int, h: Int) = (512, 512)
  val bi = new BufferedImage(w, h, BufferedImage.TYPE_INT_ARGB)

  val contexts = ai.dragonfly.bitfrost.ColorContext.knownContexts

  for (context <- contexts) {
    import context.*

    println(s"\t$context:")

    val GMG: GamutMeshGenerator = GamutMeshGenerator(context)

    def noisyImage(space:Space[_], transform: Vector3 => ColorContext.sRGB.ARGB32):Unit = {

      for (y <- 0 until h) {
        for (x <- 0 until w) {
          bi.setRGB(x, y, transform(Vector3(space.random().asInstanceOf[Model[_]].toXYZ.values)).argb)
        }
      }

      val fileName = s"./demo/image/$context$space.png"
      println(s"\t\tWriting $fileName")
      ImageIO.write(bi, "PNG", new File(fileName))
    }

    for (space <- Seq[Space[_]](XYZ, RGB, CMY, CMYK, Lab, Luv, HSV, HSL)) { // ARGB32, RGBA32, ARGB64, RGBA64)) { //

      space match {
        case perceptualSpace: GMG.ws.PerceptualSpace[_] =>
          val fg: ColorGamutVolumeMesh = GMG.fullGamut(perceptualSpace)
          PLY.writeMesh(
            fg.mesh,
            new java.io.FileOutputStream( new File(s"./demo/ply/$context${perceptualSpace}FullGamut.ply" ) ),
            (cv:Vector3) => sRGB.ARGB32(fg.vertexColorMapper(cv).argb)
          )
        case _ =>
      }

      val ug: ColorGamutVolumeMesh = GMG.usableGamut(space)
      PLY.writeMesh(
        ug.mesh,
        new java.io.FileOutputStream( new File(s"./demo/ply/$context$space.ply") ),
        (cv:Vector3) => sRGB.ARGB32(ug.vertexColorMapper(cv).argb)
      )

      noisyImage(space, GMG.XYZtoARGB32)
    }

  }

}
