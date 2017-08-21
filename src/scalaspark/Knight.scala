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

object Knight extends App {
  
  type Square = (Int, Int)
  
  /*
   * returns the list of squares that a knight can jump to from a square a n x n chessboard 
   */
  def jumps(n: Int, sq: Square): Seq[Square] = {
    def add(s1: Square, s2: Square): Square = (s1._1 + s2._1, s1._2 + s2._2)
    def isInsideBoard(sq: Square): Boolean = sq._1 > 0 && sq._2 > 0 &&
      sq._1 <= n && sq._2 <= n
      
    for (
      i <- -2 to 2; // Knight moves in +-1 / +-2 steps 
      j <- -2 to 2 if math.abs(i) != math.abs(j) && i != 0 && j != 0;
      m = add(sq, (i, j)) if (isInsideBoard(m))
    ) yield m
  }
  
  /*
   * returns all paths in which a Knight can travel a board, visiting each square exactly once
   * optionally set an upper limit to the number of returned results
   * optionally restrict to 'closed' paths only where the Knight can jump from the final position back to its initial position
   */ 
  def tour(n: Int, limit: Option[Int] = None, closed: Boolean = false) = {
    val maxEntries = n * n
    
    def isDone(len: Int): Boolean = limit match {
      case None => false
      case Some(i) => len >= i
    }
    
    def isClosedPath(ls: List[Square]): Boolean = jumps(n, ls.head) contains ls.last
    
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
              val paths = for (n <- next) yield n :: x
              depthFirst(acc, paths.toList ++ xs)
            } else if (x.size == maxEntries && (!closed || isClosedPath(x))) {
              depthFirst(x :: acc, xs)
            } else {
              depthFirst(acc, xs)
            }
          }
        }
    }
    
    val seedSquares = for (i <- 1 to n; j <- 1 to n)
      yield List((i, j))

    depthFirst(List(), seedSquares.toList)
    
    // run in parallel - TODO do some tests in a multi-core machine
    // seedSquares.par map(seed => depthFirst(List(), List(seed)))
  }
  
  // some crude unit tests
  val test1 = tour(6, Some(4), true)
  println(test1)
  // List((3,2), (5,1), (6,3), (5,5), (3,4), (1,5), (3,6), (4,4), (5,6), (6,4), (5,2), (3,3), (2,5), (1,3), (2,1), (4,2), (6,1), (5,3), (6,5), (4,6), (5,4), (6,6), (4,5), (2,6), (1,4), (2,2), (4,1), (6,2), (4,3), (3,1), (1,2), (2,4), (1,6), (3,5), (2,3), (1,1)), 
  // List((3,2), (5,1), (6,3), (5,5), (3,6), (1,5), (3,4), (5,3), (6,1), (4,2), (2,1), (1,3), (2,5), (3,3), (5,2), (6,4), (5,6), (4,4), (6,5), (4,6), (5,4), (6,6), (4,5), (2,6), (1,4), (2,2), (4,1), (6,2), (4,3), (3,1), (1,2), (2,4), (1,6), (3,5), (2,3), (1,1)), 
  // List((3,2), (5,1), (6,3), (5,5), (3,6), (1,5), (3,4), (1,3), (2,1), (4,2), (6,1), (5,3), (6,5), (4,4), (5,6), (6,4), (5,2), (3,3), (2,5), (4,6), (5,4), (6,6), (4,5), (2,6), (1,4), (2,2), (4,1), (6,2), (4,3), (3,1), (1,2), (2,4), (1,6), (3,5), (2,3), (1,1)), 
  // List((3,2), (5,1), (6,3), (5,5), (3,6), (1,5), (3,4), (4,2), (6,1), (5,3), (6,5), (4,4), (5,6), (6,4), (5,2), (3,3), (2,1), (1,3), (2,5), (4,6), (5,4), (6,6), (4,5), (2,6), (1,4), (2,2), (4,1), (6,2), (4,3), (3,1), (1,2), (2,4), (1,6), (3,5), (2,3), (1,1)))

  println(">>>>")
  val test2 = tour(7, Some(2))
  println(test2)
  // List((1,7), (3,6), (1,5), (2,7), (4,6), (6,7), (7,5), (6,3), (7,1), (5,2), (7,3), (6,1), (4,2), (5,4), (6,6), (4,7), (5,5), (7,4), (5,3), (3,2), (5,1), (7,2), (6,4), (7,6), (5,7), (4,5), (3,7), (5,6), (7,7), (6,5), (4,4), (2,5), (3,3), (2,1), (1,3), (3,4), (2,6), (1,4), (2,2), (4,1), (6,2), (4,3), (3,1), (1,2), (2,4), (1,6), (3,5), (2,3), (1,1)), 
  // List((1,7), (3,6), (1,5), (2,7), (4,6), (6,7), (7,5), (5,4), (7,3), (6,1), (4,2), (6,3), (7,1), (5,2), (6,4), (7,2), (5,1), (3,2), (5,3), (7,4), (6,6), (4,7), (5,5), (7,6), (5,7), (4,5), (3,7), (5,6), (7,7), (6,5), (4,4), (2,5), (3,3), (2,1), (1,3), (3,4), (2,6), (1,4), (2,2), (4,1), (6,2), (4,3), (3,1), (1,2), (2,4), (1,6), (3,5), (2,3), (1,1)))
}
