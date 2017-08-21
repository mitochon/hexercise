package reductions

import org.scalameter._
import common._

object ParallelCountChangeRunner {

  @volatile var seqResult = 0

  @volatile var parResult = 0

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 20,
    Key.exec.maxWarmupRuns -> 40,
    Key.exec.benchRuns -> 80,
    Key.verbose -> true) withWarmer (new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val amount = 250
    val coins = List(1, 2, 5, 10, 20, 50)
    val seqtime = standardConfig measure {
      seqResult = ParallelCountChange.countChange(amount, coins)
    }
    println(s"sequential result = $seqResult")
    println(s"sequential count time: $seqtime ms")

    def measureParallelCountChange(threshold: ParallelCountChange.Threshold): Unit = {
      val fjtime = standardConfig measure {
        parResult = ParallelCountChange.parCountChange(amount, coins, threshold)
      }
      println(s"parallel result = $parResult")
      println(s"parallel count time: $fjtime ms")
      println(s"speedup: ${seqtime / fjtime}")
    }

    measureParallelCountChange(ParallelCountChange.moneyThreshold(amount))
    measureParallelCountChange(ParallelCountChange.totalCoinsThreshold(coins.length))
    measureParallelCountChange(ParallelCountChange.combinedThreshold(amount, coins))
  }
}

object ParallelCountChange {

  /**
   * Returns the number of ways change can be made from the specified list of
   *  coins for the specified amount of money.
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money < 0) 0
    else if (money == 0) 1
    else {
      coins match {
        case x :: xs =>
          if (money < x) 0
          else if (money == x) 1
          else countChange(money - x, coins) + countChange(money, xs)
        case Nil => 0
      }
    }
  }

  type Threshold = (Int, List[Int]) => Boolean

  /**
   * In parallel, counts the number of ways change can be made from the
   *  specified list of coins for the specified amount of money.
   */
  def parCountChange(money: Int, coins: List[Int], threshold: Threshold): Int = {
    if (money < 0) 0
    else if (threshold(money, coins)) {
      countChange(money, coins)
    } else {
      coins match {
        case x :: xs =>
          val (fst, snd) = parallel(
            parCountChange(money - x, coins, threshold),
            parCountChange(money, xs, threshold))
          fst + snd
        case Nil => countChange(money, coins)
      }
    }
  }

  /** Threshold heuristic based on the starting money. */
  // from the instruction: First, implement the moneyThreshold method, 
  // which creates a threshold function that returns true when the
  // amount of money is less than or equal to 2 / 3 of the starting amount
  def moneyThreshold(startingMoney: Int): Threshold =
    (i: Int, _: List[Int]) => (i * 3.0F / 2) <= startingMoney

  /** Threshold heuristic based on the total number of initial coins. */
  // from the instruction: returns a threshold function that returns 
  /// true when the number of coins is less than or equal to 
  // the 2 / 3 of the initial number of coins:
  def totalCoinsThreshold(totalCoins: Int): Threshold =
    (_: Int, xs: List[Int]) => (xs.length * 3.0F / 2 <= totalCoins)

  /** Threshold heuristic based on the starting money and the initial list of coins. */
  // returns true when the amount of money multiplied with the number of 
  // remaining coins is less than or equal to the starting money multiplied
  // with the initial number of coins divided by 2
  def combinedThreshold(startingMoney: Int, allCoins: List[Int]): Threshold = {
    (i: Int, xs: List[Int]) => (i * xs.length * 3.0F / 2) <= (startingMoney * allCoins.length * 1.0F / 2)
  }
}
