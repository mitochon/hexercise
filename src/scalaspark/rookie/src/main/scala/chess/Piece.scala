package chess

import Board._

trait Piece {
  val sq: Square
  val color: Color
  // return the set of possible next moves by a piece
  def nextMove(board: Board): Seq[Square]
}

// the initial question only ask for the Knight's moves, but whatever ... lets define it for all the pieces

case class Knight(color: Color, sq: Square) extends Piece {
  def nextMove(board: Board): Seq[Square] =
    {
      for (
        i <- -2 to 2; // moves in +-1 / +-2 steps
        j <- -2 to 2 if math.abs(i) != math.abs(j) && i != 0 && j != 0 &&
          board.isInsideBoard((sq._1 + i, sq._2 + j))
      ) yield (sq._1 + i, sq._2 + j)
    } filter (board.isEmptyOrCanBeCaptured(color, _))
}

case class King(color: Color, sq: Square) extends Piece {
  def nextMove(board: Board): Seq[Square] =
    {
      for (
        i <- -1 to 1; // moves to adjacent squares
        j <- -1 to 1 if (!(i == 0 && j == 0) &&
          board.isInsideBoard((sq._1 + i, sq._2 + j)))
      ) yield (sq._1 + i, sq._2 + j)
    } filter (board.isEmptyOrCanBeCaptured(color, _))
}

case class Bishop(color: Color, sq: Square) extends Piece {
  def nextMove(board: Board): Seq[Square] =
    {
      for (
        i <- -board.size to board.size; // moves diagonally
        j <- List(i, -i) if (!(i == 0 && j == 0) &&
          board.isInsideBoard((sq._1 + i, sq._2 + j)))
      ) yield (sq._1 + i, sq._2 + j)
    } filter (board.isEmptyOrCanBeCaptured(color, _)) filter (!board.isPathBlocked(_, sq))
}

case class Rook(color: Color, sq: Square) extends Piece {
  def nextMove(board: Board): Seq[Square] =
    {
      for (
        i <- -board.size to board.size; // moves horizontally
        if (i != 0 && board.isInsideBoard((sq._1, sq._2 + i)))
      ) yield (sq._1, sq._2 + i)
    } ++ {
      for (
        i <- -board.size to board.size; // moves vertically
        if (i != 0 && board.isInsideBoard((sq._1 + i, sq._2)))
      ) yield (sq._1 + i, sq._2)
    } filter (board.isEmptyOrCanBeCaptured(color, _)) filter (!board.isPathBlocked(_, sq))
}

case class Queen(color: Color, sq: Square) extends Piece {
  def nextMove(board: Board): Seq[Square] =
    new Bishop(color, sq).nextMove(board) ++ new Rook(color, sq).nextMove(board)
}

case class Pawn(color: Color, sq: Square, goesUp: Boolean) extends Piece {
  def nextMove(board: Board): Seq[Square] = {
    def canCapture(target: Square): Boolean = board.getPiece(target) match {
      case None => false
      case Some(p) => p.color != color
    }

    val dRow = if (goesUp) 1 else -1
    val normalMove = for (
      dCol <- -1 to 1 if (dCol == 0 || canCapture((sq._1 + dRow, sq._2 + dCol))) &&
        board.isInsideBoard((sq._1 + dRow, sq._2 + dCol))
    ) yield (sq._1 + dRow, sq._2 + dCol)

    // a pawn can skip a square under special circumstances
    val canSkipSquare = (board.size > 7 &&
      ((sq._1 == 2 && goesUp) ||
        (sq._1 == board.size - 1 && !goesUp)))
    if (canSkipSquare) (sq._1 + dRow * 2, sq._2) +: normalMove else normalMove
  }
}
