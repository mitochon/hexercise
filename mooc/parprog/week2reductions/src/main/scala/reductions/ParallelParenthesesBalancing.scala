package reductions

import scala.annotation._
import org.scalameter._
import common._

object ParallelParenthesesBalancingRunner {

  @volatile var seqResult = false

  @volatile var parResult = false

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 40,
    Key.exec.maxWarmupRuns -> 80,
    Key.exec.benchRuns -> 120,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val length = 100000000
    val chars = new Array[Char](length)
    val threshold = 10000
    val seqtime = standardConfig measure {
      seqResult = ParallelParenthesesBalancing.balance(chars)
    }
    println(s"sequential result = $seqResult")
    println(s"sequential balancing time: $seqtime ms")

    val fjtime = standardConfig measure {
      parResult = ParallelParenthesesBalancing.parBalance(chars, threshold)
    }
    println(s"parallel result = $parResult")
    println(s"parallel balancing time: $fjtime ms")
    println(s"speedup: ${seqtime / fjtime}")
  }
}

object ParallelParenthesesBalancing extends App {

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def balance(chars: Array[Char]): Boolean = {
    val tally = chars.foldLeft(Option(0))((agg, c) => {
      c match {
        case '(' => agg.map(_ + 1)
        case ')' => agg.filter(_ > 0).map(_ - 1)
        case _ => agg
      }
    })
    tally.filter(_ == 0).isDefined
  }
      
  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def parBalance(chars: Array[Char], threshold: Int): Boolean = {

    def traverse(idx: Int, until: Int, ul: Int, ur: Int): (Int, Int) = {
      var i = idx
      var (openL, openR) = (ul, ur)
      while (i < until) {
        chars(i) match {
          // keep track of 'open' left and right
          case '(' => openL += 1
          case ')' => if (openL > 0) openL -= 1 else openR += 1
          case _   => 0 // do nothing
        }
        i += 1
      }
      (openL, openR)
    }

    def reduce(from: Int, until: Int): (Int, Int) = {
      if (until - from <= threshold) {
        traverse(from, until, 0, 0)
      } else {
        val midpoint = (from + until) / 2
        val ((l1, r1), (l2, r2)) = parallel(reduce(from, midpoint), reduce(midpoint, until))
        l1.compare(r2) match {
          case 0 => (l2, r1)
          case 1 => (l2 + l1 - r2, r1)
          case -1 => (l2, r1 + r2 - l1)
        }
      }
    }

    reduce(0, chars.length) == (0, 0)
  }
  // For those who want more:
  // Prove that your reduction operator is associative!

}
