object ninetynine1 {

  // Q1 - Find the last element of a list.
  def last[T](ls: List[T]) = ls.last              //> last: [T](ls: List[T])T
  last(List(1, 1, 2, 3, 5, 8))                    //> res0: Int = 8

  // Q2 -  Find the last but one element of a list.
  def penultimate[T](ls: List[T]) = ls.init.last  //> penultimate: [T](ls: List[T])T
  penultimate(List(1, 1, 2, 3, 5, 8))             //> res1: Int = 5
  // Q3 - Find the Kth element of a list.
  def nth[T](k: Int, ls: List[T]) = ls(k)         //> nth: [T](k: Int, ls: List[T])T
  nth(2, List(1, 1, 2, 3, 5, 8))                  //> res2: Int = 2

  // Q4 - Find the number of elements of a list.
  def length[T](ls: List[T]) = ls.length          //> length: [T](ls: List[T])Int
  length(List(1, 1, 2, 3, 5, 8))                  //> res3: Int = 6

  // Q5 - Reverse a list.
  def reverse[T](ls: List[T]) = ls.reverse        //> reverse: [T](ls: List[T])List[T]
  reverse(List(1, 1, 2, 3, 5, 8))                 //> res4: List[Int] = List(8, 5, 3, 2, 1, 1)

  // Q6 - Find out whether a list is a palindrome
  def isPalindrome[T](ls: List[T]): Boolean = ls match {
    case Nil => false
    case x :: Nil => true
    case x :: xs => x == xs.last && (xs.init == Nil || isPalindrome(xs.init))
  }                                               //> isPalindrome: [T](ls: List[T])Boolean
  // alternate solution
  def isPalindrome2[A](ls: List[A]): Boolean = ls == ls.reverse
                                                  //> isPalindrome2: [A](ls: List[A])Boolean
  isPalindrome(List(1, 2, 3, 3, 2, 1))            //> res5: Boolean = true

  // Q7 - Flatten a nested list structure.
  // unable to solve. this is the official solution
  def flatten(ls: List[Any]): List[Any] = ls flatMap {
    case i: List[_] => flatten(i)
    case e => List(e)
  }                                               //> flatten: (ls: List[Any])List[Any]
  flatten(List(List(1, 1), 2, List(3, List(5, 8))))
                                                  //> res6: List[Any] = List(1, 1, 2, 3, 5, 8)
  // Q8 - Eliminate consecutive duplicates of list elements.
  def compress[T](ls: List[T]): List[T] = {
    def compress0[T](prev: T, tail: List[T]): List[T] = tail match {
      case Nil => prev :: Nil
      case x :: xs => if (prev == x) compress0(x, xs) else prev :: compress0(x, xs)
    }
    if (ls.isEmpty) List() else compress0(ls.head, ls.tail)
  }                                               //> compress: [T](ls: List[T])List[T]
  // the official solution is much nicer
  def compressFunctional[A](ls: List[A]): List[A] =
    ls.foldRight(List[A]()) { (h, r) =>
      if (r.isEmpty || r.head != h) h :: r
      else r
    }                                             //> compressFunctional: [A](ls: List[A])List[A]
  def compressRecursive[T](ls: List[T]): List[T] = ls match {
    case Nil => Nil
    case x :: xs => x :: compressRecursive(ls.dropWhile(_ == x))
  }                                               //> compressRecursive: [T](ls: List[T])List[T]

  compress(List('a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
                                                  //> res7: List[Symbol] = List('a, 'b, 'c, 'a, 'd, 'e)

  // Q9 - Pack consecutive duplicates of list elements into sublists.
  def pack[T](ls: List[T]): List[List[T]] = {
    def append[T](elm: T, ls: List[List[T]]): List[List[T]] = ls match {
      case x :: xs => {
        (List(elm) ::: x) :: xs
      }
    }
    def pack0[T](acc: List[List[T]], tail: List[T]): List[List[T]] = tail match {
      case Nil => acc.reverse
      case x :: xs => if (acc.head(0) == x) pack0(append(x, acc), xs) else pack0(List(x) :: acc, xs)
    }
    if (ls.isEmpty) List(List()) else pack0(List(List(ls.head)), ls.tail)
  }                                               //> pack: [T](ls: List[T])List[List[T]]

  pack(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
                                                  //> res8: List[List[Symbol]] = List(List('a, 'a, 'a, 'a), List('b), List('c, 'c
                                                  //| ), List('a, 'a), List('d), List('e, 'e, 'e, 'e))

  // alternate solution
  def packFunctional[A](ls: List[A]): List[List[A]] =
    ls.foldRight(List[List[A]]()) { (h, r) =>
      if (r.isEmpty || r.head(0) != h) List(h) :: r
      else (List(h) ::: r.head) :: r.tail
    }                                             //> packFunctional: [A](ls: List[A])List[List[A]]

  packFunctional(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
                                                  //> res9: List[List[Symbol]] = List(List('a, 'a, 'a, 'a), List('b), List('c, 'c
                                                  //| ), List('a, 'a), List('d), List('e, 'e, 'e, 'e))

	// official solution - using 'span'
  def packSpan[A](ls: List[A]): List[List[A]] = {
    if (ls.isEmpty) List(List())
    else {
      val (packed, next) = ls span { _ == ls.head }
      if (next == Nil) List(packed)
      else packed :: packSpan(next)
    }
  }                                               //> packSpan: [A](ls: List[A])List[List[A]]
  
  // Q10 - Run-length encoding of a list.
  def encode[T](ls: List[T]): List[(Int, T)] = {
  	packFunctional(ls) map (x => (x.length, x.head))
  }                                               //> encode: [T](ls: List[T])List[(Int, T)]
  encode(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
                                                  //> res10: List[(Int, Symbol)] = List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (
                                                  //| 4,'e))
  
  // Q11 - Modified run-length encoding.
  def encodeModified[T](ls: List[T]): List[Any] = {
  	packFunctional(ls) map (x => if (x.length == 1) x.head else (x.length, x.head))
  }                                               //> encodeModified: [T](ls: List[T])List[Any]
  encodeModified(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
                                                  //> res11: List[Any] = List((4,'a), 'b, (2,'c), (2,'a), 'd, (4,'e))

	// official version - using 'Either'
  def encodeModified2[A](ls: List[A]): List[Either[A, (Int, A)]] =
    encode(ls) map { t => if (t._1 == 1) Left(t._2) else Right(t) }
                                                  //> encodeModified2: [A](ls: List[A])List[Either[A,(Int, A)]]
                                                                                                  
}