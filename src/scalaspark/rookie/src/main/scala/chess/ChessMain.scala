package chess

/**
 * Question: 
 * For a given Chess board, 
 * create a function that will return a set of pieces 
 * that can be captured by a Knight
 * 
 * Answer:
 * Broken down into 3 subtasks
 * 1. Design a module for Chess board => Board, Piece, etc
 * 2. Design an API for returning such a set => Board.getTakeablePieces()
 * 3. Design Test cases => ChessSuite under /test folder
 * 
 */


/** Helper class for debugging */

object ChessMain extends App {

  val queen = Queen(Black, (1, 4))
  val bishop = Bishop(White, (6, 6))
  val bishop2 = Bishop(Black, (3, 6))

  val rook = Rook(White, (3, 3))
  val knight = Knight(White, (0, 4))
  val pawn = Pawn(Black, (2, 3), true)
  val b = new Board(8, List(bishop, bishop2, pawn))
  
  b.drawMoves(bishop)
  b.drawMoves(rook)
  
  println(b.getTakeablePieces(rook))
}