package ai.dragonfly.bitfrost.color.space

import ai.dragonfly.bitfrost.cie.WorkingSpace
import ai.dragonfly.bitfrost.color.*
import ai.dragonfly.bitfrost.color.model.*
import ai.dragonfly.bitfrost.color.model.rgb.RGB

type ColorSpace[CM <: ColorModel, WS <: WorkingSpace] = CM with WS

type sRGB = ColorSpace[RGB, ai.dragonfly.bitfrost.context.sRGB.type]


