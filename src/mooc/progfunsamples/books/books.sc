package week6

object books {
      trait IntSet
      case object Empty extends IntSet
      case class Incl(s: IntSet, x: Int) extends IntSet

      def contains(s: IntSet, x: Int): Boolean = s match {
        case Empty => false
        case Incl(s, x1) =>
          if (x == x1) true
          else contains(s, x)
      }                                           //> contains: (s: week6.books.IntSet, x: Int)Boolean

	case class Book(title: String, authors: List[String])
  val books = Set (
    Book(
      title = "Structure and Interpretation of Computer Programs",
      authors = List("Abelson, Harald", "Sussman, Gerald J.")),
    Book(
      title = "Introduction to Functional Programming",
      authors = List("Bird, Richard", "Wadler, Phil")),
    Book(
      title = "Effective Java",
      authors = List("Bloch, Joshua")),
    Book(
      title = "Effective Java2",
      authors = List("Bloch, Joshua")),
    Book(
      title = "Java Puzzlers",
      authors = List("Bloch, Joshua", "Gafter, Neal")),
    Book(
      title = "Programming in Scala",
      authors = List("Odersky, Martin", "Spoon, Lex", "Venners, Bill")))
                                                  //> books  : scala.collection.immutable.Set[week6.books.Book] = Set(Book(Progra
                                                  //| mming in Scala,List(Odersky, Martin, Spoon, Lex, Venners, Bill)), Book(Stru
                                                  //| cture and Interpretation of Computer Programs,List(Abelson, Harald, Sussman
                                                  //| , Gerald J.)), Book(Effective Java2,List(Bloch, Joshua)), Book(Effective Ja
                                                  //| va,List(Bloch, Joshua)), Book(Introduction to Functional Programming,List(B
                                                  //| ird, Richard, Wadler, Phil)), Book(Java Puzzlers,List(Bloch, Joshua, Gafter
                                                  //| , Neal)))
      for {
        b1 <- books
        b2 <- books
        if b1 != b2
        a1 <- b1.authors
        a2 <- b2.authors
        if a1 == a2
      } yield a1                                  //> res0: scala.collection.immutable.Set[String] = Set(Bloch, Joshua)

}