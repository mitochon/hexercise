package scalashop

import org.scalameter._
import common._

object VerticalBoxBlurRunner {

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 5,
    Key.exec.maxWarmupRuns -> 10,
    Key.exec.benchRuns -> 10,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val radius = 3
    val width = 1920
    val height = 1080
    val src = new Img(width, height)
    val dst = new Img(width, height)
    val seqtime = standardConfig measure {
      VerticalBoxBlur.blur(src, dst, 0, width, radius)
    }
    println(s"sequential blur time: $seqtime ms")

    val numTasks = 32
    val partime = standardConfig measure {
      VerticalBoxBlur.parBlur(src, dst, numTasks, radius)
    }
    println(s"fork/join blur time: $partime ms")
    println(s"speedup: ${seqtime / partime}")
  }

}

/** A simple, trivially parallelizable computation. */
object VerticalBoxBlur {

  /**
   * Blurs the columns of the source image `src` into the destination image
   *  `dst`, starting with `from` and ending with `end` (non-inclusive).
   *
   *  Within each column, `blur` traverses the pixels by going from top to
   *  bottom.
   */
  def blur(src: Img, dst: Img, from: Int, end: Int, radius: Int): Unit = {
    for (xIdx <- from until end) {
      for (yIdx <- 0 until src.height) {
        val pixel = boxBlurKernel(src, xIdx, yIdx, radius)
        val clampedX = clamp(xIdx, 0, dst.width - 1)
        val clampedY = clamp(yIdx, 0, dst.height - 1)
        dst.update(clampedX, clampedY, pixel)
      }
    }
  }

  /** Blurs the columns of the source image in parallel using `numTasks` tasks.
   *
   *  Parallelization is done by stripping the source image `src` into
   *  `numTasks` separate strips, where each strip is composed of some number of
   *  columns.
   */
  def parBlur(src: Img, dst: Img, numTasks: Int, radius: Int): Unit = {
    val clampedNumTasks = clamp(numTasks, 1, src.width)
    val taskIndices =
      if (src.width % clampedNumTasks == 0) {
        val stripSize = src.width / clampedNumTasks
        (0 to src.width by stripSize)
      } else {
        val stripSize = (src.width / clampedNumTasks) + 1
        (0 to src.width by stripSize) :+ src.width
      }
      
    val pairedIndices = taskIndices.zip(taskIndices.drop(1))
    val tasks = pairedIndices.map { r => 
      val (start, end) = r
      task { blur(src, dst, start, end, radius) }
    }
    tasks.foreach(_.join)
  }
}
