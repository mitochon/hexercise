package sbox

import scala.annotation.tailrec

/*
 * Knight's Tour problem
 * "How can a knight jump on an N x N chessboard in such a way that it visits every square exactly once?"
 *
 * Hints:
 * - Represent the squares by pairs of their coordinates of the form (X, Y), where both X and Y are integers between 1 and N.
 * - Alternately, define a Point class for the same purpose.
 * - Write a function jumps(N, (X, Y)) to list the squares that a knight can jump to from (X, Y) on a N x N chessboard. 
 * - And finally, represent the solution of our problem as a list of knight positions (the Knight's Tour).
 * 
 * Bonus:
 * - It will take the computer a long time to find all tours at once. Can you make a lazy list that only calculates the tours as needed?
 * - Can you find only "closed tours", where the knight can jump from its final position back to its starting position?
 * - Is your program using all available cores on your computer?
 * 
 */

object Knight2 extends App {

  type Square = (Int, Int)
  
  /*
   * the set of moves a Knight can make, i.e +-1 / +-2 steps
   */
  val knightMoves = for (
    i <- -2 to 2; j <- -2 to 2 if math.abs(i) != math.abs(j) && i != 0 && j != 0
  ) yield (i, j)
  
    
  /*
   * returns the list of squares that a knight can jump to from a square a n x n chessboard 
   */
  def jumps(n: Int, sq: Square): Seq[Square] = {
    def add(s1: Square, s2: Square): Square = (s1._1 + s2._1, s1._2 + s2._2)
    def isInsideBoard(sq: Square): Boolean = sq._1 > 0 && sq._2 > 0 &&
      sq._1 <= n && sq._2 <= n

    knightMoves map (add(sq, _)) filter (isInsideBoard(_))
  }
  
  /*
   * returns a list of possible (non-exhaustive) paths in which a Knight can travel a board, visiting each square exactly once
   * optionally set an upper limit to the number of returned results
   * optionally restrict to 'closed' paths only where the Knight can jump from the final position back to its initial position
   */ 
  def tour(n: Int, limit: Option[Int] = None, closed: Boolean = false) = {
    val maxEntries = n * n
    var count = 0
    
    def isDone(len: Int): Boolean = limit match {
      case None => false
      case Some(i) => count >= i
    }
    
    def isClosedPath(ls: List[Square]): Boolean = jumps(n, ls.head) contains ls.last
    
    def uniquesCount(a: Seq[Square], b: Seq[Square]): Int = (a.toSet -- b.toSet).size
    
    /*
     * computes score based on Warnsdorff's rule
     * reference http://en.wikipedia.org/wiki/Knight%27s_tour#Warnsdorff.27s_rule
     */
    def warnsdorffScore(ls: List[Square]): Int = ls match {
      case x :: xs => uniquesCount(jumps(n, x), xs)
      case _ => 0
    }
    
    @tailrec
    def depthFirst(acc: List[List[Square]], rem: List[List[Square]]): List[List[Square]] = 
      if (isDone(acc.length)) acc
      else {
        rem match {
          case Nil => acc
          case x :: xs => {
            val (y, ys) = (x.head, x.tail)
            val next = jumps(n, y).toSet -- ys.toSet // no duplicates
            if (next.size > 0) {
              val paths = for (n <- next.toList) yield n :: x
              val sortedPaths = paths map (p => (p, warnsdorffScore(p))) sortBy (_._2) // order by score
              val warnsdorffOptimalScore = sortedPaths.head._2
              val warnsdorffPaths = sortedPaths takeWhile (_._2 == warnsdorffOptimalScore) map { case (p, s) => p }
              depthFirst(acc, warnsdorffPaths ++ xs)
            } else if (x.size == maxEntries && (!closed || isClosedPath(x))) {
              count += 1 // not thread-safe
              depthFirst(x :: acc, xs)
            } else {
              depthFirst(acc, xs)
            }
          }
        }
    }
    
    val seedSquares = for (i <- 1 to n; j <- 1 to n)
      yield List((i, j))
    
    // run in parallel
    val paths = seedSquares.par map(seed => depthFirst(List(), List(seed)))

    // since count is not thread-safe, double-check limit for accuracy
    if (limit.isEmpty) paths.flatten
    else paths.flatten.take(limit.get)
  }
  
  // some crude unit tests
  val test1 = tour(8, Some(3), true)
  println(test1)
  println(">>>>")

  val test2 = tour(32, Some(4))
  println(test2)
  println(">>>>")
  
  val test3 = tour(60, Some(6))
  println(test3)
}
