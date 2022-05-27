package ai.dragonfly.bitfrost.color.spectral

import ai.dragonfly.math.vector.Vector3

/**
 * CIE 1931 CMF data revised by Judd in 1951
 * 10 nano meter wavelength steps
 * Source: http://www.cvrl.org/cmfs.htm
 */

object CIE1931_JUDD1951_2deg_10nm extends SampleSet {
  override val samples:Array[Sample] = Array[Sample](
    Sample(370, Vector3(0.0008, 0.0001, 0.0046)),
      Sample(380, Vector3(0.0045, 0.0004, 0.0224)),
      Sample(390, Vector3(0.0201, 0.0015, 0.0925)),
      Sample(400, Vector3(0.0611, 0.0045, 0.2799)),
      Sample(410, Vector3(0.1267, 0.0093, 0.5835)),
      Sample(420, Vector3(0.2285, 0.0175, 1.0622)),
      Sample(430, Vector3(0.3081, 0.0273, 1.4526)),
      Sample(440, Vector3(0.3312, 0.0379, 1.6064)),
      Sample(450, Vector3(0.2888, 0.0468, 1.4717)),
      Sample(460, Vector3(0.2323, 0.06, 1.288)),
      Sample(470, Vector3(0.1745, 0.091, 1.1133)),
      Sample(480, Vector3(0.092, 0.139, 0.7552)),
      Sample(490, Vector3(0.0318, 0.208, 0.4461)),
      Sample(500, Vector3(0.0048, 0.323, 0.2644)),
      Sample(510, Vector3(0.0093, 0.503, 0.1541)),
      Sample(520, Vector3(0.0636, 0.71, 0.0763)),
      Sample(530, Vector3(0.1668, 0.862, 0.0412)),
      Sample(540, Vector3(0.2926, 0.954, 0.02)),
      Sample(550, Vector3(0.4364, 0.995, 0.0088)),
      Sample(560, Vector3(0.597, 0.995, 0.0039)),
      Sample(570, Vector3(0.7642, 0.952, 0.002)),
      Sample(580, Vector3(0.9159, 0.87, 0.0016)),
      Sample(590, Vector3(1.0225, 0.757, 0.0011)),
      Sample(600, Vector3(1.0544, 0.631, 0.0007)),
      Sample(610, Vector3(0.9922, 0.503, 0.0003)),
      Sample(620, Vector3(0.8432, 0.381, 0.0002)),
      Sample(630, Vector3(0.6327, 0.265, 0.0001)),
      Sample(640, Vector3(0.4404, 0.175, 0)),
      Sample(650, Vector3(0.2787, 0.107, 0)),
      Sample(660, Vector3(0.1619, 0.061, 0)),
      Sample(670, Vector3(0.0858, 0.032, 0)),
      Sample(680, Vector3(0.0459, 0.017, 0)),
      Sample(690, Vector3(0.0222, 0.0082, 0)),
      Sample(700, Vector3(0.0113, 0.0041, 0)),
      Sample(710, Vector3(0.0057, 0.0021, 0)),
      Sample(720, Vector3(0.0028, 0.0011, 0)),
      Sample(730, Vector3(0.0015, 0.0005, 0)),
      Sample(740, Vector3(0.0005, 0.0002, 0)),
      Sample(750, Vector3(0.0003, 0.0001, 0)),
      Sample(760, Vector3(0.0002, 0.0001, 0)),
      Sample(770, Vector3(0.0001, 0, 0))
  )

}
