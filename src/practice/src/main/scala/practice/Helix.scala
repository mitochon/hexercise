package practice

/**
 * Given a 2D integer array representing a completely filled sudoku puzzle, implement a function which validates the puzzle according to the 3 rules:
 * 1) Every row must contain all integer values 1-9 once and only once
 * 2) Every column must contain all integer values 1-9 once and only once
 * 3) Every 3x3 subsquare must contain all integer values 1-9 once and only once
 *
 * Test cases:
 *
 * [[5,1,3,6,8,7,2,4,9],
 * [8,4,9,5,2,1,6,3,7],
 * [2,6,7,3,4,9,5,8,1],
 * [1,5,8,4,6,3,9,7,2],
 * [9,7,4,2,1,8,3,6,5],
 * [3,2,6,7,9,5,4,1,8],
 * [7,8,2,9,3,4,1,5,6],
 * [6,3,5,1,7,2,8,9,4],
 * [4,9,1,8,5,6,7,2,3]]
 * true
 *
 * [[2,1,3,6,8,7,2,4,9],
 * [8,4,9,5,2,1,6,3,7],
 * [2,6,7,3,4,9,5,8,1],
 * [1,5,8,4,6,3,3,7,2],
 * [9,2,4,2,1,8,3,6,5],
 * [3,2,6,1,9,5,4,1,8],
 * [7,8,2,9,3,4,1,5,6],
 * [6,3,5,1,7,2,8,9,4],
 * [4,9,1,8,5,8,7,2,3]]
 * false
 *
 * Seq(
 * Seq(5,1,3,6,8,7,2,4,9),
 * Seq(8,4,9,5,2,1,6,3,7),
 * Seq(2,6,7,3,4,9,5,8,1),
 * Seq(1,5,8,4,6,3,9,7,2),
 * Seq(9,7,4,2,1,8,3,6,5),
 * Seq(3,2,6,7,9,5,4,1,8),
 * Seq(7,8,2,9,3,4,1,5,6),
 * Seq(6,3,5,1,7,2,8,9,4),
 * Seq(1,9,1,8,5,6,7,2,3))
 * false
 *
 * Seq(
 * Seq(1,5,2,9,3,7,4,8,6),
 * Seq(2,9,3,7,4,8,6,1,5),
 * Seq(8,6,1,5,2,9,3,7,4),
 * Seq(9,3,7,4,8,6,1,5,2),
 * Seq(7,4,8,6,1,5,2,9,3),
 * Seq(3,7,4,8,6,1,5,2,9),
 * Seq(5,2,9,3,7,4,8,6,1),
 * Seq(4,8,6,1,5,2,9,3,7),
 * Seq(6,1,5,2,9,3,7,4,8))
 * false
 *
 */

object Helix {
  val vals1to9 = (1 to 9).toList

  def has1To9(xs: Seq[Int]): Boolean = {
    xs.sorted == vals1to9
  }

  def checkRow(input: Seq[Seq[Int]]): Boolean = {
    input.forall(has1To9)
  }

  def checkColumn(input: Seq[Seq[Int]]): Boolean = {
    var valid = true
    for (i <- 0 to 8) {
      val ys = input.flatMap(xs => Seq(xs(i)))
      // should've been val ys = input.map(_(i))
      valid = valid && has1To9(ys)
    }
    valid
  }

  def checkGrid(input: Seq[Seq[Int]]): Boolean = {

    def checkGrid147(start: Int, end: Int): Boolean = {
      val xs = (start to end).flatMap(i => input(i).take(3))
      has1To9(xs)
    }

    def checkGrid258(start: Int, end: Int): Boolean = {
      val xs = (start to end).flatMap(i => input(i).drop(3).take(3))
      has1To9(xs)
    }

    def checkGrid369(start: Int, end: Int): Boolean = {
      val xs = (start to end).flatMap(i => input(i).drop(6).take(3))
      has1To9(xs)
    }

    def checkGridX(start: Int, end: Int): Boolean = {
      val xs = (start to end).flatMap(i => input(i).drop(6).take(3))
      has1To9(xs)
    }

    checkGrid147(0, 2) && checkGrid147(3, 5) && checkGrid147(6, 8) &&
      checkGrid258(0, 2) && checkGrid258(3, 5) && checkGrid258(6, 8) &&
      checkGrid369(0, 2) && checkGrid369(3, 5) && checkGrid369(6, 8)
  }

  def validateSudoku(input: Seq[Seq[Int]]): Boolean = {
    checkRow(input) && checkColumn(input) && checkGrid(input)
  }

  def main(args: Array[String]) {

    val result = validateSudoku(
      Seq(
        Seq(5, 1, 3, 6, 8, 7, 2, 4, 9),
        Seq(8, 4, 9, 5, 2, 1, 6, 3, 7),
        Seq(2, 6, 7, 3, 4, 9, 5, 8, 1),
        Seq(1, 5, 8, 4, 6, 3, 9, 7, 2),
        Seq(9, 7, 4, 2, 1, 8, 3, 6, 5),
        Seq(3, 2, 6, 7, 9, 5, 4, 1, 8),
        Seq(7, 8, 2, 9, 3, 4, 1, 5, 6),
        Seq(6, 3, 5, 1, 7, 2, 8, 9, 4),
        Seq(4, 9, 1, 8, 5, 6, 7, 2, 3)))
    println(result)

    val result2 = validateSudoku(
      Seq(
        Seq(2, 1, 3, 6, 8, 7, 2, 4, 9),
        Seq(8, 4, 9, 5, 2, 1, 6, 3, 7),
        Seq(2, 6, 7, 3, 4, 9, 5, 8, 1),
        Seq(1, 5, 8, 4, 6, 3, 3, 7, 2),
        Seq(9, 2, 4, 2, 1, 8, 3, 6, 5),
        Seq(3, 2, 6, 1, 9, 5, 4, 1, 8),
        Seq(7, 8, 2, 9, 3, 4, 1, 5, 6),
        Seq(6, 3, 5, 1, 7, 2, 8, 9, 4),
        Seq(4, 9, 1, 8, 5, 8, 7, 2, 3)))

    println(result2)

    val result3 = validateSudoku(Seq(
      Seq(5, 1, 3, 6, 8, 7, 2, 4, 9),
      Seq(8, 4, 9, 5, 2, 1, 6, 3, 7),
      Seq(2, 6, 7, 3, 4, 9, 5, 8, 1),
      Seq(1, 5, 8, 4, 6, 3, 9, 7, 2),
      Seq(9, 7, 4, 2, 1, 8, 3, 6, 5),
      Seq(3, 2, 6, 7, 9, 5, 4, 1, 8),
      Seq(7, 8, 2, 9, 3, 4, 1, 5, 6),
      Seq(6, 3, 5, 1, 7, 2, 8, 9, 4),
      Seq(1, 9, 1, 8, 5, 6, 7, 2, 3)))

    println(result3)

    val result4 = validateSudoku(
      Seq(
        Seq(1, 5, 2, 9, 3, 7, 4, 8, 6),
        Seq(2, 9, 3, 7, 4, 8, 6, 1, 5),
        Seq(8, 6, 1, 5, 2, 9, 3, 7, 4),
        Seq(9, 3, 7, 4, 8, 6, 1, 5, 2),
        Seq(7, 4, 8, 6, 1, 5, 2, 9, 3),
        Seq(3, 7, 4, 8, 6, 1, 5, 2, 9),
        Seq(5, 2, 9, 3, 7, 4, 8, 6, 1),
        Seq(4, 8, 6, 1, 5, 2, 9, 3, 7),
        Seq(6, 1, 5, 2, 9, 3, 7, 4, 8)))
    println(result4)
  }
}
