package practice

object ArrayTricks extends App {

  /**
   * We have our lists of orders sorted numerically already, in arrays. Write
   * a function to merge our arrays of orders into one sorted array.
   *
   * For example:
   * my_array = [3,4,6,10,11,15]
   * alices_array = [1,5,8,12,14,19]
   *
   * print merge_arrays(my_array, alices_array)
   * # prints
   * [1,3,4,5,6,8,10,11,12,14,15,19]
   */
  def merge(xs: Array[Int], ys: Array[Int]): Array[Int] = {
    var i, j = 0
    var zs = Array[Int]()

    while (i < xs.length && j < ys.length) {
      if (xs(i) < ys(j)) {
        zs = zs :+ xs(i)
        i = i + 1
      } else {
        zs = zs :+ ys(j)
        j = j + 1
      }
    }
    while (i < xs.length) {
      zs = zs :+ xs(i)
      i = i + 1
    }
    while (j < ys.length) {
      zs = zs :+ ys(j)
      j = j + 1
    }
    zs
  }

  /**
   * Given an array_of_ints, find the highest_product you can get from three
   * of the integers. The input array_of_ints will always have at least three
   * integers.
   *
   * Solution - use greedy algorithm
   * 1. keep track of top 2 and at each i keep checking if currentMax > top 2 * currentVal
   * 2. need to check bottom 2 to check for negative number, e.g [-10, -10, 1, 3, 2]
   *
   * slightly cleaner variant - keep track of 'product' of top 2 and 'product' of bottom 2
   *
   */
  def max3(f: Array[Int]): Int = {
    var max1 = f(0)
    var max2 = f(0) * f(1)
    var max3 = f(0) * f(1) * f(2)

    var min1 = f(0)
    var min2 = f(0) * f(1)

    for (i <- 1 to f.length - 1) {
      if (i > 2) {
        max3 = Math.max(max3, Math.max(f(i) * max2, f(i) * min2))
      }
      if (i > 1) {
        max2 = Math.max(max2, f(i) * max1)
        min2 = Math.min(min2, f(i) * min1)
      }
      max1 = Math.max(max1, f(i))
      min1 = Math.min(min1, f(i))
    }
    max3
  }

  /**
   * You have an array of integers, and for each index you want to find the
   * product of every integer except the integer at that index. Write a
   * function get_products_of_all_ints_except_at_index() that takes an array
   * of integers and returns an array of the products. For example, given: [1,
   * 7, 3, 4] your function would return: [84, 12, 28, 21] by calculating:
   * [7*3*4, 1*3*4, 1*7*4, 1*7*3] Do not use division in your solution.
   *
   * Solution - use greedy algorithm - overlapping subproblems
   * 1. the value at i = product( product all values before i * product of all values after i)
   * 2. get the product of all values before i in 1 pass
   * 3. get the product of all values after i in 1 pass
   * 4. multiply the resulting array -> this can be done using 1 array
   *
   * e.g. getProduct(new int[] {1, 2, 6, 5, 9})
   */
  def multiply(xs: Array[Int]): Array[Int] = {
    val accLeft = xs.scanLeft(1)(_ * _)
    val accRight = xs.scanRight(1)(_ * _).drop(1)
    accLeft.zip(accRight).map {
      case (a, b) => a * b
    }
  }

  println(s"merge ${merge(Array(3, 4, 6, 10, 11, 15), Array(1, 5, 8, 12, 14, 19)).mkString(",")}")
  println(s"max3 ${max3(Array(-10, -10, 1, 3, 2, 7))}")
  println(s"multiply ${multiply(Array(1, 7, 3, 4)).mkString(",")}")
}