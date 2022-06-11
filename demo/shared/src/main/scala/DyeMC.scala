import ai.dragonfly.bitfrost.ColorContext.sRGB.ARGB32
import ai.dragonfly.bitfrost.color.model.rgb.discrete
import ai.dragonfly.math.stats.DenseHistogramOfContinuousDistribution
import ai.dragonfly.math.stats.probability.distributions.stream.StreamingVectorStats
import ai.dragonfly.math.vector.*
import bridge.array.*
import scalatags.Text.all.*

import scala.::
import scala.collection.mutable
import scala.language.postfixOps

object DyeMC extends App {

  val N:Int = 7

  val dyeColors:ARRAY[ARGB32] = ARRAY[ARGB32](
    // primary:
    ARGB32(0xFF1D1D21), // Black
    ARGB32(0xFFB02E26), // Red
    ARGB32(0xFF5E7C16), // Green
    ARGB32(0xFF835432), // Brown
    ARGB32(0xFF3C44AA), // Blue
    ARGB32(0xFFFED83D), // Yellow
    ARGB32(0xFFF9FFFE), // White
    // secondary:
    ARGB32(0xFF8932B8), // Purple
    ARGB32(0xFF169C9C), // Cyan
    ARGB32(0xFF9D9D97), // Light gray
    ARGB32(0xFF474F52), // Gray
    ARGB32(0xFFF38BAA), // Pink
    ARGB32(0xFF80C71F), // Lime
    ARGB32(0xFF3AB3DA), // Light blue
    ARGB32(0xFFC74EBD), // Magenta
    ARGB32(0xFFF9801D) // Orange
  )

  val bounds:VectorBounds = {
    val svs:StreamingVectorStats = StreamingVectorStats(3)
    dyeColors.foreach((c:ARGB32) => svs(ARGB32.toVector3(c)))
    svs.bounds()
  }

//  var unreachable:Int = 0
//  var reachable:Int = 0
//  var total:Int = 0
//  print(s"\n[")
//  for (i <- 1 to 1 + (255 / 4)) print("⠀")
//  print("]\n[")
//  for (red <- 0 to 255) {
//    if ((red - 1) % 4 == 0) print("⣿")
//    for (green <- 0 to 255) {
//      for (blue <- 0 to 255) {
//        if (bounds.contains(Vector3(red, green, blue))) reachable += 1
//        else unreachable += 1
//        total += 1
//      }
//    }
//  }
//
//  println(s"]\nunreachable + reachable = total : $unreachable + $reachable = $total == ${unreachable + reachable}")

  val depthHistogram:DenseHistogramOfContinuousDistribution = DenseHistogramOfContinuousDistribution(N, 0.0, N)

  val memoization:mutable.HashMap[ARGB32, List[ARGB32]] = mutable.HashMap[ARGB32, List[ARGB32]]()

  def memoize(dc:ARGB32, path:List[ARGB32], depth:Int = 0): Unit = {
    val startSize:Int = memoization.size
    if (path.size < N) {
      var nextSequences:List[StainedGlassSequence] = List[StainedGlassSequence]()
      for (pc <- dyeColors) {
        val mix: ARGB32 = ARGB32.weightedAverage(dc, 0.5, pc, 0.5)
        memoization.get(mix) match {
          case Some(oldPath: List[ARGB32]) if oldPath.size < path.size + 1 => // keep old path
          case _ =>
            val newPath: List[ARGB32] = pc :: path
            memoization.put(mix, newPath)
            depthHistogram(depth)
            nextSequences = StainedGlassSequence(mix, newPath) :: nextSequences
        }
      }

      nextSequences.foreach( (sgs:StainedGlassSequence) => memoize(sgs.approximateColor, sgs.sequence, depth + 1))
      //println(s"memoize($dc, path(${path.length})) added ${memoization.size - startSize} colors to the cache.")

    }
  }

  // build up the cache

  for (dc <- dyeColors) memoization.put(dc, List[ARGB32](dc)) // add primary and secondary dye colors

  for (dc <- dyeColors) { // search exhaustively from each primary
    memoize(dc, memoization.getOrElse(dc, List[ARGB32](dc)))
  }

  println(s"N = $N: memoized ${memoization.size} reachable colors")

  println(pre(depthHistogram.toString))

  def makeBeacon(target: ARGB32): StainedGlassSequence = {
    memoization.get(target) match {
      case Some(path:List[ARGB32]) => StainedGlassSequence(target, path)
      case None =>
        val candidates = memoization.keys
        var nearestMatch: ARGB32 = candidates.head
        var minError: Double = ARGB32.distanceSquared(target, nearestMatch)
        for (candidate <- candidates.tail) {
          val error = ARGB32.distanceSquared(target, candidate)
          if (error < minError) {
            minError = error
            nearestMatch = candidate
          }
        }
        makeBeacon(nearestMatch)
    }
  }



  val histogram:DenseHistogramOfContinuousDistribution = DenseHistogramOfContinuousDistribution(15, 0.0, 1.0)



    //<span style="display: inline-block; background-color: #1e51ca; border: 1px solid var(--theme-border-color); border-radius: 50%; width: 1em; height: 1em; vertical-align: text-top;">

    def colorDotScale(c:ARGB32):String = {
      s"display: inline-block; background-color: ${c.html()}; border: 1px solid var(--theme-border-color); border-radius: 50%; width: 1em; height: 1em; vertical-align: text-top;"
    }

    def tenMore() = {
      (0 until 10) map { _ =>
        val tc: ARGB32 = ARGB32.random()
        val reachable: Boolean = memoization.get(tc) match {
          case Some(_) => true
          case _ => false
        }
        val sgs: StainedGlassSequence = makeBeacon(tc)
        val result: ARGB32 = sgs.approximateColor

        if (reachable) histogram(tc.similarity(result))
        td(style := "vertical-align: top;")(
        table(
          tr(
            td(backgroundColor := tc.html(), `width` := "50px;")((0 until height).map(_ => br())),
            td(backgroundColor := result.html(), `width` := "50px;")(
              raw("&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;"),
              (0 until height - sgs.sequence.size).map(_ => br())
            )
          ),
          tr(
            td(
              if (reachable) "✅ " else "❌",
              br(),
              f"${100.0 * ARGB32.similarity(tc, result)}%.1f%%",
              br(),
              span(s"N = ${sgs.sequence.size}")
            ),
            td(
              sgs.sequence.map(
                c => div(
                  span(style := colorDotScale(c))(br()),
                  raw("&nbsp;"),
                  img(src := s"./image/mcdye/${c.html().substring(1)}.png")
                )
              )
            ),
            td(raw("&nbsp;"))
          )
        )
        )
      }
    }

    val height:Int = N+3
    println(
      div(
        table(
          tr( tenMore() ),
          tr( tenMore() ),
          tr( tenMore() )
        )
      )
    )


  println(pre(histogram.toString))

}

case class StainedGlassSequence(approximateColor:ARGB32, sequence:List[ARGB32])