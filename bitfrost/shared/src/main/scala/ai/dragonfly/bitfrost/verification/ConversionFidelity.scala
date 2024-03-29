package ai.dragonfly.bitfrost.verification

import ai.dragonfly.bitfrost.ColorContext

object ConversionFidelity extends App {
  for (ctx <- ColorContext.knownContexts) {
    import ctx.*

    var `error(ARGB32<->RGB)`: Double = 0.0
    var `error(ARGB32<->RGBA32)`: Double = 0.0
    var `error(RGB<->CMYK)`: Double = 0.0
    var `error(RGB<->CMY)`: Double = 0.0
    var `error(RGB<->HSV)`: Double = 0.0
    var `error(RGB<->HSL)`: Double = 0.0
    var `error(RGB<->XYZ)`: Double = 0.0
    var `error(RGB<->Lab)`: Double = 0.0
    var `error(RGB<->Luv)`: Double = 0.0
    // 16,777,216 iterations:
    print(s"$ctx\n[")
    for (i <- 1 to 1 + (255 / 4)) print("⠀")
    print("]\n[")
    for (red <- 0 to 255) {
      if ((red - 1) % 4 == 0) print("⣿")
      for (green <- 0 to 255) {
        for (blue <- 0 to 255) {
          val c = ARGB32(255, red, green, blue)
          // ARGB -> RGB -> ARGB
          val rgb = c.toRGB
          var err = 1.0 - c.similarity(ARGB32.fromRGB(rgb))
          if (err != 0.0) {
            println(s"$c ${ARGB32.fromRGB(rgb)} $err")
            `error(ARGB32<->RGB)` += err
          }
          // ARGB -> RGB -> ARGB
          val rgba: RGBA32 = RGBA32.fromRGB(rgb)
          err = 1.0 - c.similarity(ARGB32.fromRGB(rgba.toRGB))
          if (err != 0.0) {
            println(s"$c $err")
            `error(ARGB32<->RGBA32)` += err
          }
          // ARGB -> RGB -> CMY -> RGB -> ARGB
          val cmy = CMY.fromRGB(rgb)
          var cT = ARGB32.fromRGB(cmy.toRGB)
          err = 1.0 - c.similarity(cT)
          if (err != 0.0) {
            println(s"$c -> $cmy -> $cT: $err")
            `error(RGB<->CMY)` += err
          }
          // ARGB -> RGB -> CMYK -> RGB -> ARGB
          val cmyk = CMYK.fromRGB(rgb)
          cT = ARGB32.fromRGB(cmyk.toRGB)
          err = 1.0 - c.similarity(cT)
          if (err != 0.0) {
            println(s"$c -> $cmyk -> $cT: $err")
            `error(RGB<->CMYK)` += err
          }
          // ARGB -> RGB -> HSV -> RGB -> ARGB
          val hsv = HSV.fromRGB(rgb)
          cT = ARGB32.fromRGB(hsv.toRGB)
          err = 1.0 - c.similarity(cT)
          if (err != 0.0) {
            println(s"$c -> $hsv -> $cT: $err")
            `error(RGB<->HSV)` += err
          }
          // ARGB -> RGB -> HSL -> RGB -> ARGB
          val hsl = HSL.fromRGB(rgb)
          cT = ARGB32.fromRGB(hsl.toRGB)
          err = 1.0 - c.similarity(cT)
          if (err != 0.0) {
            println(s"$c -> $hsl -> $cT: $err")
            `error(RGB<->HSL)` += err
          }
          // ARGB -> RGB -> XYZ -> RGB -> ARGB
          val xyz = rgb.toXYZ
          cT = ARGB32.fromRGB(RGB.fromXYZ(xyz))
          err = 1.0 - c.similarity(cT)
          if (err != 0.0) {
            println(s"$c -> $xyz -> $cT: $err")
            `error(RGB<->XYZ)` += err
          }
          // ARGB -> RGB -> XYZ -> Lab -> XYZ -> RGB -> ARGB
          val lab = Lab.fromXYZ(xyz)

          cT = ARGB32.fromRGB(RGB.fromXYZ(lab.toXYZ))
          err = 1.0 - c.similarity(cT)
          if (err != 0.0) {
            println(s"$c -> $lab -> $cT: $err")
            `error(RGB<->Lab)` += err
          }

          // ARGB -> RGB -> XYZ -> Luv -> XYZ -> RGB -> ARGB
          val luv = Luv.fromXYZ(xyz)
          cT = ARGB32.fromRGB(RGB.fromXYZ(luv.toXYZ))
          err = 1.0 - c.similarity(cT)
          if (err != 0.0) {
            println(s"$c -> $luv -> $cT: $err")
            `error(RGB<->Luv)` += err
          }
        }
      }
    }
    println("]")
    println(s"${`error(ARGB32<->RGB)`} `error(ARGB32<->RGB)`")
    println(s"${`error(ARGB32<->RGBA32)`} `error(ARGB32<->RGBA32)`")
    println(s"${`error(RGB<->CMY)`} `error(RGB<->CMY)`")
    println(s"${`error(RGB<->CMYK)`} `error(RGB<->CMYK)`")
    println(s"${`error(RGB<->HSV)`} `error(RGB<->HSV)`")
    println(s"${`error(RGB<->HSL)`} `error(RGB<->HSL)`")
    println(s"${`error(RGB<->XYZ)`} `error(RGB<->XYZ)`")
    println(s"${`error(RGB<->Lab)`} `error(RGB<->Lab)`")
    println(s"${`error(RGB<->Luv)`} `error(RGB<->Luv)`")

  }
}
