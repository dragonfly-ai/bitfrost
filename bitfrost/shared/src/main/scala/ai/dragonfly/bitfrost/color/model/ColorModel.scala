package ai.dragonfly.bitfrost.color.model

import ai.dragonfly.bitfrost.cie.WorkingSpace

trait ColorModel {

}

trait ProvidedColorModels extends WorkingSpace with CMYK with HSL with HSV with Lab with Luv
