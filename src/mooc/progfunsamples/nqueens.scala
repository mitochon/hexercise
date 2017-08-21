package week6

object pairs {

  val s = (1 to 6).toSet                          //> s  : scala.collection.immutable.Set[Int] = Set(5, 1, 6, 2, 3, 4)
  
  s map (_ / 2)                                   //> res0: scala.collection.immutable.Set[Int] = Set(2, 0, 3, 1)
  
  s contains 5                                    //> res1: Boolean = true
  
  def isPrime(n: Int) = (2 until n) forall (n % _ != 0)
                                                  //> isPrime: (n: Int)Boolean
  val n = 8                                       //> n  : Int = 8
  val sublists =
    (1 until n) flatMap (i =>
      (1 until i) map (j => (i, j)))              //> sublists  : scala.collection.immutable.IndexedSeq[(Int, Int)] = Vector((2,1)
                                                  //| , (3,1), (3,2), (4,1), (4,2), (4,3), (5,1), (5,2), (5,3), (5,4), (6,1), (6,2
                                                  //| ), (6,3), (6,4), (6,5), (7,1), (7,2), (7,3), (7,4), (7,5), (7,6))
  sublists filter { case (x, y) => isPrime(x + y) }
                                                  //> res2: scala.collection.immutable.IndexedSeq[(Int, Int)] = Vector((2,1), (3,2
                                                  //| ), (4,1), (4,3), (5,2), (6,1), (6,5), (7,4), (7,6))

  for {
    i <- 1 until n
    j <- 1 until i
    if isPrime(i + j)
  } yield (i, j)                                  //> res3: scala.collection.immutable.IndexedSeq[(Int, Int)] = Vector((2,1), (3,2
                                                  //| ), (4,1), (4,3), (5,2), (6,1), (6,5), (7,4), (7,6))

  def queens(n: Int) = {
    def placeQueens(k: Int): Set[List[Int]] = {
      if (k == 0) Set(List())
      else {
        def isSafe(col: Int, queens: List[Int]) = {
          val row = queens.length
    		  queens zip (row - 1 to 0 by -1) forall {
      		  case (c, r) => col != c && (row - r) != math.abs(col - c)
    		  }
    		}
        for {
          queens <- placeQueens(k - 1)
          col <- 0 until n
          if isSafe(col, queens)
        } yield col :: queens
      }
    }
    placeQueens(n)
  }                                               //> queens: (n: Int)Set[List[Int]]
  
  def show(queens: List[Int]) = {
    val lines =
	    for (col <- queens.reverse)
  	  yield Vector.fill(queens.length)("* ").updated(col, "X ").mkString
  	"\n" + (lines mkString "\n")
  }                                               //> show: (queens: List[Int])String
    
  (queens(8) take 10 map show) mkString "\n"      //> res4: String = "
                                                  //| * * * X * * * * 
                                                  //| * * * * * * * X 
                                                  //| X * * * * * * * 
                                                  //| * * * * X * * * 
                                                  //| * * * * * * X * 
                                                  //| * X * * * * * * 
                                                  //| * * * * * X * * 
                                                  //| * * X * * * * * 
                                                  //| 
                                                  //| * * * * X * * * 
                                                  //| * * X * * * * * 
                                                  //| X * * * * * * * 
                                                  //| * * * * * * X * 
                                                  //| * X * * * * * * 
                                                  //| * * * * * * * X 
                                                  //| * * * * * X * * 
                                                  //| * * * X * * * * 
                                                  //| 
                                                  //| * X * * * * * * 
                                                  //| * * * * X * * * 
                                                  //| * * * * * * X * 
                                                  //| X * * * * * * * 
                                                  //| * * X * * * * * 
                                                  //| * * * * * * * X 
                                                  //| * * * * * X * * 
                                                  //| * * * X * * * * 
                                                  //| 
                                                  //| * * * * * * X * 
                                                  //| * * * X * * * * 
                                                  //| * X * * * * * * 
                                                  //| * * * * * * * X 
                                                  //| * * * * * X * * 
                                                  //| X * * * * * * * 
                                                  //| * * X * * * * * 
                                                  //| * * * * X * * * 
                                                  //| 
                                                  //| * * * * * X * * 
                                                  //| * * X * * * * * 
                                                  //| * * * * * * X * 
                                                  //| * X * * * * * * 
                                                  //| * * * X * * * * 
                                                  //| * * * * * * * X 
                                                  //| X * * * * * * * 
                                                  //| * * * * X * * * 
                                                  //| 
                                                  //| * * * * * X * * 
                                                  //| * * X * * * * * 
                                                  //| * * * * * * X * 
                                                  //| * * * X * * * * 
                                                  //| X * * * * * * * 
                                                  //| * * * * * * * X 
                                                  //| * X * * * * * * 
                                                  //| * * * * X * * * 
                                                  //| 
                                                  //| * * * * * X * * 
                                                  //| * * * X * * * * 
                                                  //| X * * * * * * * 
                                                  //| * * * * X * * * 
                                                  //| * * * * * * * X 
                                                  //| * X * * * * * * 
                                                  //| * * * * * * X * 
                                                  //| * * X * * * * * 
                                                  //| 
                                                  //| * * * * X * * * 
                                                  //| * * * * * * X * 
                                                  //| * X * * * * * * 
                                                  //| * * * X * * * * 
                                                  //| * * * * * * * X 
                                                  //| X * * * * * * * 
                                                  //| * * X * * * * * 
                                                  //| * * * * * X * * 
                                                  //| 
                                                  //| * X * * * * * * 
                                                  //| * * * * * X * * 
                                                  //| X * * * * * * * 
                                                  //| * * * * * * X * 
                                                  //| * * * X * * * * 
                                                  //| * * * * * * * X 
                                                  //| * * X * * * * * 
                                                  //| * * * * X * * * 
                                                  //| 
                                                  //| * * * * * X * * 
                                                  //| * * * X * * * * 
                                                  //| * X * * * * * * 
                                                  //| * * * * * * * X 
                                                  //| * * * * X * * * 
                                                  //| * * * * * * X * 
                                                  //| X * * * * * * * 
                                                  //| * * X * * * * * "
  
  def isSafe(row: Int, col: Int, queens: List[Int]) =
    queens zip (row - 1 to 0 by -1) forall {
      case (r, c) => col != c && (row - r) != math.abs(col - c)
    }                                             //> isSafe: (row: Int, col: Int, queens: List[Int])Boolean
}