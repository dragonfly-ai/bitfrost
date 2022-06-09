package ai.dragonfly.bitfrost.visualization

import ai.dragonfly.bitfrost.ColorContext
import ai.dragonfly.bitfrost.cie.{ChromaticAdaptation, WorkingSpace}
import ai.dragonfly.bitfrost.color.model.rgb.RGB
import ai.dragonfly.bitfrost.color.model.huesat.*
import ai.dragonfly.bitfrost.color.model.subtractive.*
import ai.dragonfly.bitfrost.color.model.perceptual.*
import ai.dragonfly.math.vector.Vector3

import java.io.File
import java.io.OutputStream

class GamutMeshGenerator(val ws:WorkingSpace) {

  lazy val fullGamutXYZ:ws.Gamut = ws.Gamut.fromSpectralSamples(
    ws.cmf,
    (v:Vector3) => Vector3(
      ws.whitePoint.x * v.x,
      ws.whitePoint.y * v.y,
      ws.whitePoint.z * v.z
    )
  )

  val XYZtoARGB32:Vector3 => ColorContext.sRGB.ARGB32 = {
    import ColorContext.sRGB
    if (ws == sRGB.ARGB32) {
      (v: Vector3) => sRGB.ARGB32.fromXYZ(sRGB.XYZ(v.values))
    } else {
      val chromaticAdapter: ChromaticAdaptation[ws.type, sRGB.type] = ChromaticAdaptation[ws.type, sRGB.type](ws, sRGB)
      (v: Vector3) => sRGB.ARGB32.fromXYZ(chromaticAdapter(ws.XYZ(v.values)))
    }
  }

  def fullGamut(space:WorkingSpace#PerceptualSpace[_]):ColorGamutVolumeMesh = space match {
    case _:ws.XYZ.type => ColorGamutVolumeMesh( fullGamutXYZ.volumeMesh, XYZtoARGB32 )
    case wsSpace:ws.PerceptualSpace[_] => ColorGamutVolumeMesh(
        fullGamutXYZ.volumeMesh.transform(
          (v: Vector3) => wsSpace.toVector3(wsSpace.fromXYZ(ws.XYZ(v.values)))
        ),
        (v:Vector3) => XYZtoARGB32(Vector3(space.fromVector3(v).toXYZ.values))
      )
    case _ => throw Exception("Wrong Working Space!")
  }

  def usableGamut(space:WorkingSpace#Space[_]):ColorGamutVolumeMesh = ColorGamutVolumeMesh(
    space match {
      case perceptualSpace: ws.PerceptualSpace[_] => perceptualSpace.gamut.volumeMesh
      case _: ws.VectorSpace[_] => VolumeMesh.cube()
      case _: ws.CylindricalSpace[_] =>
        try {
          if (space == ws.asInstanceOf[HSL].HSL) VolumeMesh.cylinder(sideSegments = 64)
          else if (space == ws.asInstanceOf[HSV].HSV) VolumeMesh.cylinder(capSegments = 6)
          else null
        } catch {
          case _ => try {
            if (space == ws.asInstanceOf[HSV].HSV) VolumeMesh.cylinder(capSegments = 6)
            else null
          } catch { _ => null}
        }
    },
    (v:Vector3) => XYZtoARGB32(Vector3(space.fromVector3(v).toXYZ.values))
  )

}

case class ColorGamutVolumeMesh(mesh:VolumeMesh, vertexColorMapper:Vector3 => ColorContext.sRGB.ARGB32)