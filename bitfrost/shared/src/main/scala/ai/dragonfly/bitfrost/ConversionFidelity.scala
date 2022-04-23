package ai.dragonfly.bitfrost

import ai.dragonfly.bitfrost.cie.XYZ

object ConversionFidelity extends App {
  for (ctx <- context.knownContexts.take(1)) {
    import ctx.*

    var `error(ARGB<->NRGB)`: Double = 0.0
    var `error(NRGB<->CMYK)`: Double = 0.0
    var `error(NRGB<->HSV)`: Double = 0.0
    var `error(NRGB<->HSL)`: Double = 0.0
    var `error(NRGB<->XYZ)`: Double = 0.0
    var `error(NRGB<->Lab)`: Double = 0.0
    var `error(NRGB<->Luv)`: Double = 0.0
    // 16,777,216 iterations:
    print(s"$ctx\n[")
    for (i <- 1 to 1 + (255 / 4)) print("⠀")
    print("]\n[")
    for (red <- 0 to 255) {
      if ((red - 1) % 4 == 0) print("⣿")
      for (green <- 0 to 255) {
        for (blue <- 0 to 255) {
          val c = ARGB(255, red, green, blue)
          // ARGB -> NRGB -> ARGB
          val cN = c.toNRGB
          var err = c.similarity(cN.toARGB)
          if (err > 0.0) {
            println(s"$c $err")
            `error(ARGB<->NRGB)` += err
          }
          // ARGB -> NRGB -> CMYK -> NRGB -> ARGB
          val cmyk = CMYK.fromNRGB(cN)
          var cT = cmyk.toNRGB.toARGB
          err = c.similarity(cT)
          if (err > 0.0) {
            println(s"$c -> $cmyk -> $cT: $err")
            `error(NRGB<->CMYK)` += err
          }
          // ARGB -> NRGB -> HSV -> NRGB -> ARGB
          val hsv = HSV.fromNRGB(cN)
          cT = hsv.toNRGB.toARGB
          err = c.similarity(cT)
          if (err > 0.0) {
            println(s"$c -> $hsv -> $cT: $err")
            `error(NRGB<->HSV)` += err
          }
          // ARGB -> NRGB -> HSL -> NRGB -> ARGB
          val hsl = HSL.fromNRGB(cN)
          cT = hsl.toNRGB.toARGB
          err = c.similarity(cT)
          if (err > 0.0) {
            println(s"$c -> $hsl -> $cT: $err")
            `error(NRGB<->HSL)` += err
          }
          // ARGB -> NRGB -> XYZ -> NRGB -> ARGB
          val xyz = cN.toXYZ
          cT = XYZ.toNRGB(ctx)(xyz).toARGB
          err = c.similarity(cT)
          if (err > 0.0) {
            println(s"$c -> $xyz -> $cT: $err")
            `error(NRGB<->XYZ)` += err
          }
//          // ARGB -> NRGB -> XYZ -> Lab -> XYZ -> NRGB -> ARGB
//          val lab = Lab.fromXYZ(xyz)
//          cT = XYZ.toNRGB(ctx)(lab.toXYZ).toARGB
//          err = c.similarity(cT)
//          if (err > 0.0) {
//            println(s"$c -> $lab -> $cT: $err")
//            `error(NRGB<->Lab)` += err
//          }
          // ARGB -> NRGB -> XYZ -> Luv -> XYZ -> NRGB -> ARGB
          val luv = Luv.fromXYZ(xyz)
          cT = XYZ.toNRGB(ctx)(luv.toXYZ).toARGB
          err = c.similarity(cT)
          if (err > 0.0) {
            println(s"$c -> $luv -> $cT: $err")
            `error(NRGB<->Luv)` += err
          }
        }
      }
    }
    println("]")
    println(s"${`error(ARGB<->NRGB)`} `error(ARGB<->NRGB)`")
    println(s"${`error(NRGB<->CMYK)`} `error(NRGB<->CMYK)`")
    println(s"${`error(NRGB<->HSV)`} `error(NRGB<->HSV)`")
    println(s"${`error(NRGB<->HSL)`} `error(NRGB<->HSL)`")
    println(s"${`error(NRGB<->XYZ)`} `error(NRGB<->XYZ)`")
    println(s"${`error(NRGB<->Lab)`} `error(NRGB<->Lab)`")
    println(s"${`error(NRGB<->Luv)`} `error(NRGB<->Luv)`")

  }
}
