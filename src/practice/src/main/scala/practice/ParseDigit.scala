package practice

/**
 * Revisit old problem
 *
 * If 1 -> 'a', 2 -> 'b', etc, find the size of possibilities for decoding a given string, e.g
 *
 * "11" -> decodes to aa, k. The function should return 2.
 * "111" decodes to aaa, ak, ka. The function should return 3.
 * "31" decodes to ca. The function should return 1
 * "321" decodes to cba, cu. The function should return 2.
 * "101" decodes to ja. The function should return 1.
 * "010" decodes to nothing because a leading zero is not valid. The function should return 0
 * "111100001" decodes to nothing because multiple consecutive zeros is not valid. This function should return 0
 *
 */
object ParseDigit extends App {

  val validEntries = (1 to 26).map(_.toString)

  def isValid(digits: String): Boolean = validEntries.contains(digits)

  def parseRecursive(digits: String): Int = {
    def parseAt(index: Int, str: String): Int = {
      if (str.length < index) 0
      else {
        val (left, right) = str.splitAt(index)
        if (!isValid(left)) 0
        else if (right.isEmpty) 1
        else parseRecursive(right)
      }
    }

    Option(digits).fold(0)(d => parseAt(1, d) + parseAt(2, d))
  }

  def parseIterative(digits: String): Int = {
    var memoized = collection.mutable.Map[String, Int]()

    def parseAt(index: Int, str: String): Int = {
      val (left, right) = str.splitAt(index)
      if (!isValid(left)) 0
      else if (right.isEmpty) 1
      else memoized(right)
    }

    val buildupList = digits.scanRight(Seq.empty[Char])(_ +: _)
      .reverse.drop(1) // drop the empty element
      .map(_.mkString) // convert Seq[Char] to String

    for (e <- buildupList) {
      val sum =
        if (e.length == 1) parseAt(1, e)
        else parseAt(1, e) + parseAt(2, e)

      memoized.put(e, sum)
    }
    memoized.getOrElse(digits, 0)
  }

  // sample inputs and their expected results
  val test = Map("" -> 0, "11" -> 2, "111" -> 3, "31" -> 1, "321" -> 2, "101" -> 1, "010" -> 0, "111100001" -> 0)

  test.foreach {
    case (k, v) => {
      println(s"(recursive) $k -> actual: ${parseRecursive(k)} expected: $v")
      println(s"(iterative) $k -> actual: ${parseIterative(k)} expected: $v")
    }
  }
}