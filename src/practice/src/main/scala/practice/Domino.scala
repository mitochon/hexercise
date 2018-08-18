package practice

import scala.annotation.tailrec

/**
 * Write a program for matching domino cards
 * 1. First write a class for domino.
 * 2. Write a method for finding the longest chain given a set of dominos.
 * 3. First do step 2 with the assumption that a domino has a fixed Left and Right part.
 */

case class Domino(left: Int, right: Int) { // can add some validation in the body

  // check if the other domino can be matched against this one
  def matchingFaceStrict(other: Domino): Boolean = {
    right == other.left
  }

  def matchingFace(other: Domino): Boolean = {
    right == other.left || right == other.right // TODO: need to flip
  }
}

object Domino extends App {

  val r = new java.util.Random()

  // make a random card
  def make: Domino = {
    Domino(r.nextInt(6) + 1, r.nextInt(6) + 1)
  }

  // get the longest chain from a list of dominoes
  def buildChain(done: List[Domino], remaining: List[Domino]): Seq[Domino] = {
    if (remaining.isEmpty) done
    else {
      val possibleMatches =
        if (done.isEmpty) remaining
        else remaining.filter(d => done.last.matchingFaceStrict(d))
      val chains = possibleMatches.map(d => buildChain(done :+ d, (remaining.toSet - d).toList))
      chains.sortBy(-_.length).headOption.getOrElse(done)
    }
  }

  def run(n: Int) = {
    val input = Range(0, n).map(_ => make).toList
    input.foreach(println)

    val output = buildChain(List(), input)
    output.foreach(println)
  }

  val y = List(
    Domino(3, 6),
    Domino(6, 4),
    Domino(4, 5),
    Domino(2, 1),
    Domino(3, 3),
    Domino(5, 1))

  val foo = buildChain(List(), y.toList)
  println(foo)
  //List(Domino(3,3), Domino(3,6), Domino(6,4), Domino(4,5), Domino(5,1))

}