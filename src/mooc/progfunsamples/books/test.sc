package week6

object test {
  val n = 7                                       //> n  : Int = 7
  val pairs = (1 until n) map (i =>
    (1 until i) map (j => (i, j)))                //> pairs  : scala.collection.immutable.IndexedSeq[scala.collection.immutable.In
                                                  //| dexedSeq[(Int, Int)]] = Vector(Vector(), Vector((2,1)), Vector((3,1), (3,2))
                                                  //| , Vector((4,1), (4,2), (4,3)), Vector((5,1), (5,2), (5,3), (5,4)), Vector((6
                                                  //| ,1), (6,2), (6,3), (6,4), (6,5)))
  (pairs foldRight Seq[(Int, Int)]())(_ ++ _)     //> res0: Seq[(Int, Int)] = Vector((2,1), (3,1), (3,2), (4,1), (4,2), (4,3), (5,
                                                  //| 1), (5,2), (5,3), (5,4), (6,1), (6,2), (6,3), (6,4), (6,5))

  case class Book(title: String, authors: List[String])
  
  val books: List[Book] = Nil                     //> books  : List[week6.test.Book] = List()
  
      val bookSet = books.toSet                   //> bookSet  : scala.collection.immutable.Set[week6.test.Book] = Set()
      for {
        b1 <- bookSet
        b2 <- bookSet
        if b1 != b2
        a1 <- b1.authors
        a2 <- b2.authors
        if a1 == a2
      } yield a1                                  //> res1: scala.collection.immutable.Set[String] = Set()
      }