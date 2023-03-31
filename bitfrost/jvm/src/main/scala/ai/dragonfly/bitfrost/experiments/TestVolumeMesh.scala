package ai.dragonfly.bitfrost.experiments

import ai.dragonfly.mesh.io.PLY
import ai.dragonfly.bitfrost.visualization.ColorGamutVolumeMesh
import ai.dragonfly.math.vector.Vector3
import ai.dragonfly.mesh.*
import ai.dragonfly.mesh.shape.*

import java.io.File

import scala.language.implicitConversions

object TestVolumeMesh extends App {

  import ai.dragonfly.mesh.Mesh.*
  import ai.dragonfly.bitfrost.ColorContext.sRGB.*

  val sRGB_RGB_TestCube: ColorGamutVolumeMesh = ColorGamutVolumeMesh(
    Cube(),
    (v: Vector3) => ARGB32.fromRGB(RGB(v.values)).argb
  )
  PLY.writeMesh(
    sRGB_RGB_TestCube.mesh,
    new java.io.FileOutputStream( new File(s"./demo/ply/primitives/sRGB_RGB_TestCube.ply")),
    (c:Vector3) => sRGB.ARGB32(sRGB_RGB_TestCube.vertexColorMapper(c))
  )

  val sRGB_HSV_TestCylinder: ColorGamutVolumeMesh = ColorGamutVolumeMesh(
    Cylinder(capSegments = 4),
    (v: Vector3) => ARGB32.fromRGB(HSV.fromVector3(v).toRGB)
  )

  PLY.writeMesh(
    sRGB_HSV_TestCylinder.mesh,
    new java.io.FileOutputStream( new File(s"./demo/ply/primitives/sRGB_HSV_TestCylinder.ply") ),
    (c:Vector3) => sRGB.ARGB32(sRGB_HSV_TestCylinder.vertexColorMapper(c))
  )

  val sRGB_HSL_TestCylinder: ColorGamutVolumeMesh = ColorGamutVolumeMesh(
    Cylinder(sideSegments = 10),
    (v:Vector3) => ARGB32.fromRGB(HSL.fromVector3(v).toRGB)
  )
  PLY.writeMesh(
    sRGB_HSL_TestCylinder.mesh,
    new java.io.FileOutputStream( new File(s"./demo/ply/primitives/sRGB_HSL_TestCylinder.ply") ),
    (c:Vector3) => sRGB.ARGB32(sRGB_HSL_TestCylinder.vertexColorMapper(c))
  )

}