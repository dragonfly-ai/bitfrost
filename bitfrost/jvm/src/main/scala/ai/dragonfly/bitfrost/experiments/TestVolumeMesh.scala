package ai.dragonfly.bitfrost.experiments

import ai.dragonfly.bitfrost.visualization.{ColorGamutVolumeMesh, PLY}
import ai.dragonfly.math.vector.Vector3

import java.io.File

object TestVolumeMesh extends App {

  import ai.dragonfly.bitfrost.visualization.VolumeMesh.{cube, cylinder}
  import ai.dragonfly.bitfrost.ColorContext.sRGB.*

  PLY.write(
    ColorGamutVolumeMesh(
      cube(),
      (v:Vector3) => ARGB32.fromRGB(RGB(v.values))
    ),
    new java.io.FileOutputStream( new File(s"./demo/ply/primitives/sRGB_RGB_TestCube.ply"))
  )

  PLY.write(
    ColorGamutVolumeMesh(
      cylinder(capSegments = 4),
      (v:Vector3) => ARGB32.fromRGB(HSV.fromVector3(v).toRGB)
    ),
    new java.io.FileOutputStream( new File(s"./demo/ply/primitives/sRGB_HSV_TestCylinder.ply") )
  )
  PLY.write(
    ColorGamutVolumeMesh(
      cylinder(sideSegments = 10),
      (v:Vector3) => ARGB32.fromRGB(HSL.fromVector3(v).toRGB)
    ),
    new java.io.FileOutputStream( new File(s"./demo/ply/primitives/sRGB_HSL_TestCylinder.ply") )
  )

}