package ai.dragonfly.bitfrost.color.spectral

import ai.dragonfly.math.vector.Vector3

/**
 * CIE 2006 CMF data revised for bitfrost.
 * These data consist primarily of the 5 nano meter wavelength step data.
 * It also includes extrema from 0.1 nano meter wavelength step data substituted for its nearest 5 nano meter wavelength.
 * 5 nano meter wavelength steps
 * Source: http://www.cvrl.org/cmfs.htm
 */

object HYBRID_CIE2006_2deg_5nm_WITH_1pm_EXTREMA extends SampleSet {
  override val samples:Array[Sample] = Array[Sample](
    Sample(390, Vector3(0.003769647, 0.0004146161, 0.0184726)),
    Sample(395, Vector3(0.009382967, 0.001059646, 0.04609784)),
    Sample(400, Vector3(0.02214302, 0.002452194, 0.109609)),
    Sample(405, Vector3(0.04742986, 0.004971717, 0.2369246)),
    Sample(410, Vector3(0.08953803, 0.00907986, 0.4508369)),
    Sample(415, Vector3(0.1446214, 0.01429377, 0.7378822)),
    Sample(420, Vector3(0.2035729, 0.02027369, 1.051821)),
    Sample(425, Vector3(0.2488523, 0.02612106, 1.305008)),
    Sample(430, Vector3(0.2918246, 0.03319038, 1.552826)),
    Sample(435, Vector3(0.3227087, 0.0415794, 1.74828)),
    //`λ->XYZ`( 440, Vector3(0.3482554, 0.05033657, 1.917479 ) ),
    Sample(442.1, Vector3(0.3487819, 0.05346281, 1.934853)),
    //`λ->XYZ`( 445, Vector3(0.3418483, 0.05743393, 1.918437 ) ),
    Sample(450, Vector3(0.3224637, 0.06472352, 1.848545)),
    Sample(455, Vector3(0.2826646, 0.07238339, 1.664439)),
    Sample(460, Vector3(0.2485254, 0.08514816, 1.522157)),
    Sample(465, Vector3(0.2219781, 0.1060145, 1.42844)),
    Sample(470, Vector3(0.1806905, 0.1298957, 1.25061)),
    Sample(475, Vector3(0.129192, 0.1535066, 0.9991789)),
    Sample(480, Vector3(0.08182895, 0.1788048, 0.7552379)),
    Sample(485, Vector3(0.04600865, 0.2064828, 0.5617313)),
    Sample(490, Vector3(0.02083981, 0.237916, 0.4099313)),
    Sample(495, Vector3(0.007097731, 0.285068, 0.3105939)),
    Sample(500, Vector3(0.002461588, 0.3483536, 0.2376753)),
    Sample(505, Vector3(0.003649178, 0.4277595, 0.1720018)),
    Sample(510, Vector3(0.01556989, 0.5204972, 0.1176796)),
    Sample(515, Vector3(0.04315171, 0.6206256, 0.08283548)),
    Sample(520, Vector3(0.07962917, 0.718089, 0.05650407)),
    Sample(525, Vector3(0.1268468, 0.7946448, 0.03751912)),
    Sample(530, Vector3(0.1818026, 0.8575799, 0.02438164)),
    Sample(535, Vector3(0.2405015, 0.9071347, 0.01566174)),
    Sample(540, Vector3(0.3098117, 0.9544675, 0.00984647)),
    Sample(545, Vector3(0.3804244, 0.9814106, 0.006131421)),
    Sample(550, Vector3(0.4494206, 0.9890228, 0.003790291)),
    //Sample( 555, Vector3(0.5280233, 0.9994608, 0.002327186 ) ),
    Sample(556.1, Vector3(0.5465651, 1, 0.002090846)),
    //Sample(560, Vector3(0.6133784, 0.9967737, 0.001432128)),
    Sample(565, Vector3(0.7016774, 0.9902549, 0.0008822531)),
    Sample(570, Vector3(0.796775, 0.9732611, 0.0005452416)),
    Sample(575, Vector3(0.8853376, 0.9424569, 0.0003386739)),
    Sample(580, Vector3(0.9638388, 0.8963613, 0.0002117772)),
    Sample(585, Vector3(1.051011, 0.8587203, 0.0001335031)),
    //`λ->XYZ`( 590, Vector3(1.109767, 0.8115868, 8.494468E-05 ) ),
    Sample(591.4, Vector3(1.121677, 0.7963719, 7.497919E-05)),
    Sample(595, Vector3(1.14362, 0.7544785, 5.460706E-05)),
    Sample(599.1, Vector3(1.15133, 0.7033109, 3.832554E-05)),
    //`λ->XYZ`( 600, Vector3(1.151033, 0.6918553, 3.549661E-05 ) ),
    Sample(605, Vector3(1.134757, 0.6270066, 2.334738E-05)),
    Sample(610, Vector3(1.083928, 0.5583746, 1.554631E-05)),
    Sample(615, Vector3(1.007344, 0.489595, 1.048387E-05)),
    Sample(620, Vector3(0.9142877, 0.4229897, 0)),
    Sample(625, Vector3(0.8135565, 0.3609245, 0)),
    Sample(630, Vector3(0.6924717, 0.2980865, 0)),
    Sample(635, Vector3(0.575541, 0.2416902, 0)),
    Sample(640, Vector3(0.4731224, 0.1943124, 0)),
    Sample(645, Vector3(0.3844986, 0.1547397, 0)),
    Sample(650, Vector3(0.2997374, 0.119312, 0)),
    Sample(655, Vector3(0.2277792, 0.08979594, 0)),
    Sample(660, Vector3(0.1707914, 0.06671045, 0)),
    Sample(665, Vector3(0.1263808, 0.04899699, 0)),
    Sample(670, Vector3(0.09224597, 0.03559982, 0)),
    Sample(675, Vector3(0.0663996, 0.02554223, 0)),
    Sample(680, Vector3(0.04710606, 0.01807939, 0)),
    Sample(685, Vector3(0.03292138, 0.01261573, 0)),
    Sample(690, Vector3(0.02262306, 0.008661284, 0)),
    Sample(695, Vector3(0.01575417, 0.006027677, 0)),
    Sample(700, Vector3(0.01096778, 0.004195941, 0)),
    Sample(705, Vector3(0.00760875, 0.002910864, 0)),
    Sample(710, Vector3(0.005214608, 0.001995557, 0)),
    Sample(715, Vector3(0.003569452, 0.001367022, 0)),
    Sample(720, Vector3(0.002464821, 0.0009447269, 0)),
    Sample(725, Vector3(0.001703876, 0.000653705, 0)),
    Sample(730, Vector3(0.001186238, 0.000455597, 0)),
    Sample(735, Vector3(0.0008269535, 0.0003179738, 0)),
    Sample(740, Vector3(0.0005758303, 0.0002217445, 0)),
    Sample(745, Vector3(0.0004058303, 0.0001565566, 0)),
    Sample(750, Vector3(0.0002856577, 0.0001103928, 0)),
    Sample(755, Vector3(0.0002021853, 7.827442E-05, 0)),
    Sample(760, Vector3(0.000143827, 5.578862E-05, 0)),
    Sample(765, Vector3(0.0001024685, 3.981884E-05, 0)),
    Sample(770, Vector3(7.347551E-05, 2.860175E-05, 0)),
    Sample(775, Vector3(5.25987E-05, 2.051259E-05, 0)),
    Sample(780, Vector3(3.806114E-05, 1.487243E-05, 0)),
    Sample(785, Vector3(2.758222E-05, 1.080001E-05, 0)),
    Sample(790, Vector3(2.004122E-05, 7.86392E-06, 0)),
    Sample(795, Vector3(1.458792E-05, 5.736935E-06, 0)),
    Sample(800, Vector3(1.068141E-05, 4.211597E-06, 0)),
    Sample(805, Vector3(7.857521E-06, 3.106561E-06, 0)),
    Sample(810, Vector3(5.768284E-06, 2.286786E-06, 0)),
    Sample(815, Vector3(4.259166E-06, 1.693147E-06, 0)),
    Sample(820, Vector3(3.167765E-06, 1.262556E-06, 0)),
    Sample(825, Vector3(2.358723E-06, 9.422514E-07, 0)),
    Sample(830, Vector3(1.762465E-06, 7.05386E-07, 0)),
    Sample(835, Vector3(0, 0, 0))
  )

  
}