package chess

import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import Board.Square
import org.junit.runner.RunWith

@RunWith(classOf[JUnitRunner])
class ChessSuite extends FunSuite {
  
  // For this exercise we'll just focus on the Knight piece
  
  test("Knight can take 2 squares at corners") {
    List((1, 1), (1, size), (size, 1), (size, size)) map (s => 
      assert(countKnightMoves(2, s, Knight(Black, s), empty), s"Wrong square taken at $s"))
  }
  
  test("Knight can take 4 squares between corners") {
    { for (i <- 3 to size - 2) yield (i, 1) } ++
    { for (i <- 3 to size - 2) yield (1, i) } ++
    { for (i <- 3 to size - 2) yield (size, i) } ++
    { for (i <- 3 to size - 2) yield (i, size) } map (s =>
      assert(countKnightMoves(4, s, Knight(Black, s), empty), s"Wrong square taken at $s"))
  }

  test("Knight can take 8 squares in the middle") {
    { for (i <- 3 to size - 2; j <- 3 to size - 2) yield (i, j) } map (s => 
      assert(countKnightMoves(8, s, Knight(Black, s), empty), s"Wrong square taken at $s"))    
  }
  
  test("Knight cannot take piece from empty board") {
    for (i <- 1 to size; j <- 1 to size) yield assert(empty.getTakeablePieces(Knight(Black, (i, j))).size == 0)
  }
 
  test("Knight cannot take any piece of the same color") {
    for (i <- 1 to size; j <- 1 to size) yield assert(Knight(Black, (i, j)).nextMove(fullblack).size == 0, 
        s"Taken piece of same color at ($i $j)")
  }

  // TODO use scalacheck API to generate arbitrary boards and generate more test cases
  
  def emptyBoard(s: Int) = new Board(s, List())
  
  def fullBoard(s: Int, c: Color) = 
    new Board(s, for (i <- 1 to s; j <- 1 to s) yield Bishop(c, (i, j)))

  def isPlausibleKnightMove(thisSq: Square, thatSq: Square): Boolean = {
    (math.abs(thisSq._1 - thatSq._1), math.abs(thisSq._2 - thatSq._2)) match {
      case (1, 2) | (2, 1) => true
      case _ => false
    }
  }
  
  def count(i: Int, s: Square, l: Seq[Square], b: Board) = {
    l.size == i && (l.dropWhile(e => b.hasPiece(e) && isPlausibleKnightMove(s, e)).size == 0)
  }

  def countKnightMoves(i: Int, s: Square, k: Knight, b: Board) = {
    val moves = k.nextMove(b)
    moves.size == i &&
      moves.dropWhile(e =>
        b.isEmptyOrCanBeCaptured(k.color, e) &&
          isPlausibleKnightMove(s, e)).size == 0
  }

  val size = 6
  val empty = emptyBoard(size)
  val fullblack = fullBoard(size, Black)
  val fullwhite = fullBoard(size, White)

}