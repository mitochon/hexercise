package chess

sealed trait Color
object Black extends Color
object White extends Color


object Board {
  type Square = (Int, Int)
}

/** For this implementation the board does not capture mutable state */

class Board(val size: Int, val pieces: Seq[Piece]) {

  import chess.Board.Square
  
  /* validation */
  if (size < 2)
    throw new IllegalArgumentException("size < 2")

  pieces foreach (p => if (!isInsideBoard(p.sq))
    throw new IllegalArgumentException(s"Piece $p is outside the board "))

  // ... Add whatever additional rules
  val rules = Seq(UniqueSquaresRule, OneKingMaxRule)

  rules foreach (r => if (!r.check(pieces))
    throw new IllegalArgumentException("Failed " + r))

  def hasPiece(thatSq: Square): Boolean =
    getPiece(thatSq) match {
      case Some(x) => true
      case None => false
    }

  def getPiece(thatSq: Square): Option[Piece] = {
    pieces filter (p => (p.sq._1 == thatSq._1) && (p.sq._2 == thatSq._2)) match {
      case Nil => None
      case x +: xs => Some(x)
    }
  }

  // return set of Pieces on the board that can be taken by the passed piece
  def getTakeablePieces(p: Piece): Seq[Piece] = {
    val moves = p.nextMove(this)
    for (p <- pieces; m <- moves if (p.sq == m)) yield p
  }

  // return a path from this square to that square
  private def getPath(thisSq: Square, thatSq: Square): Seq[Square] =
    (thisSq._1 - thatSq._1, thisSq._2 - thatSq._2) match {
      case (0, y) => // horizontal
        if (y < 0) List.fill(-y - 1)(thisSq._1) zip (thisSq._2 + 1 until thatSq._2)
        else List.fill(y - 1)(thisSq._1) zip (thatSq._2 + 1 until thisSq._2)
      case (y, 0) => // vertical
        if (y < 0) (thisSq._1 + 1 until thatSq._1) zip List.fill(-y - 1)(thisSq._2)
        else (thatSq._1 + 1 until thisSq._1) zip List.fill(y - 1)(thisSq._2)
      case (x, y) if x > 0 => // diagonal left
        if (y < 0) (thisSq._1 - 1 until thatSq._1 by -1) zip (thisSq._2 + 1 until thatSq._2)
        else (thisSq._1 - 1 until thatSq._1 by -1) zip (thisSq._2 - 1 until thatSq._2 by -1)
      case (x, y) if x < 0 => // diagonal right
        if (y < 0) (thisSq._1 + 1 until thatSq._1) zip (thisSq._2 + 1 until thatSq._2)
        else (thisSq._1 + 1 until thatSq._1) zip (thisSq._2 - 1 until thatSq._2 by -1)
    }

  // return true if any piece between this Square and thatSquare, exclusive
  def isPathBlocked(thisSq: Square, thatSq: Square): Boolean = !{
    getPath(thisSq, thatSq) dropWhile (!hasPiece(_))
  }.isEmpty

  // return true if a Square on the board is occupied by a different color
  def isEmptyOrCanBeCaptured(color: Color, s: Square): Boolean =
    getPiece(s) match {
      case None => true // empty
      case Some(p) => p.color != color
    }

  // return true if square is inside the board
  def isInsideBoard(s: Square): Boolean = {
    s._1 > 0 && s._2 > 0 && s._1 <= size && s._2 <= size
  }

  // to help visualize the set of possible moves by a piece
  def drawMoves(piece: Piece) = {

    // initialize array
    val arr = Array.ofDim[String](size + 1, size + 1)
    for (
      i <- 0 to size;
      j <- 0 to size
    ) yield (i, j) match {
      case (0, j) => arr(0)(j) = j + " "
      case (i, 0) => arr(i)(0) = i + " "
      case (i, j) => arr(i)(j) = "O "
    }

    // mark a potential move with an 'X'
    piece.nextMove(this) map (n => arr(n._1)(n._2) = "X ")

    // mark the piece location with a '&'
    arr(piece.sq._1)(piece.sq._2) = "@ "

    // print results
    val lines = for (line <- arr) yield line.mkString
    println("\n" + (lines.reverse mkString "\n"))
  }
}


/** Set of rules governing the board */

trait BoardRule {
  def check(pieces: Seq[Piece]): Boolean
}

object UniqueSquaresRule extends BoardRule {
  def check(pieces: Seq[Piece]): Boolean =
    { pieces.map(_.sq).distinct }.length == pieces.length
}

object OneKingMaxRule extends BoardRule {
  val pred = (p: Piece) => p match {
    case King(_, _) => true
    case _ => false
  }
  def check(pieces: Seq[Piece]): Boolean =
    { pieces filter (pred(_)) }.length < 2
}

// ... Add whatever additional rules as needed