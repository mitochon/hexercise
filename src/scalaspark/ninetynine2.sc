object ninetynine2 {

  // Q12 - Decode a run-length encoded list.
  def decode[T](ls: List[(Int, T)]): List[T] = {
    def repeat[T](elm: (Int, T)): List[T] = {
      if (elm._1 == 1) List(elm._2) else elm._2 :: repeat(elm._1 - 1, elm._2)
    }
    ls flatMap (x => repeat(x))
  }                                               //> decode: [T](ls: List[(Int, T)])List[T]
  decode(List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e)))
                                                  //> res0: List[Symbol] = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e
                                                  //| , 'e)

  // official version - uses 'make' - deprecated - replace w/ 'fill'
  def decode2[T](ls: List[(Int, T)]): List[T] = {
    ls flatMap (x => List.fill(x._1)(x._2))
  }                                               //> decode2: [T](ls: List[(Int, T)])List[T]
  decode2(List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e)))
                                                  //> res1: List[Symbol] = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e
                                                  //| , 'e)
  
  // Q13 - Run-length encoding of a list (direct solution).
  def packFunctional[A](ls: List[A]): List[List[A]] =
    ls.foldRight(List[List[A]]()) { (h, r) =>
      if (r.isEmpty || r.head(0) != h) List(h) :: r
      else (List(h) ::: r.head) :: r.tail
    }                                             //> packFunctional: [A](ls: List[A])List[List[A]]

  def encodeDirect[A](ls: List[A]): List[(Int, A)] =
    ls.foldRight(List[(Int, A)]()) { (h, r) =>
      if (r.isEmpty || r.head._2 != h) (1, h) :: r
      else (r.head._1 + 1, r.head._2) :: r.tail
    }                                             //> encodeDirect: [A](ls: List[A])List[(Int, A)]
  encodeDirect(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
                                                  //> res2: List[(Int, Symbol)] = List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4
                                                  //| ,'e))

	// official solution - a bit more elegant
  def encodeDirect2[A](ls: List[A]): List[(Int, A)] =
    if (ls.isEmpty) Nil
    else {
      val (packed, next) = ls span { _ == ls.head }
      (packed.length, packed.head) :: encodeDirect(next)
    }                                             //> encodeDirect2: [A](ls: List[A])List[(Int, A)]
    
  // Q14 - Duplicate the elements of a list.
  def duplicate[T](ls: List[T]) = ls flatMap (List.fill(2)(_))
                                                  //> duplicate: [T](ls: List[T])List[T]
  duplicate(List('a, 'b, 'c, 'c, 'd))             //> res3: List[Symbol] = List('a, 'a, 'b, 'b, 'c, 'c, 'c, 'c, 'd, 'd)
  
  // Q15 - Duplicate the elements of a list a given number of times.
  def duplicateN[T](i: Int, ls: List[T]) = ls flatMap (List.fill(i)(_))
                                                  //> duplicateN: [T](i: Int, ls: List[T])List[T]
  duplicateN(3, List('a, 'b, 'c, 'c, 'd))         //> res4: List[Symbol] = List('a, 'a, 'a, 'b, 'b, 'b, 'c, 'c, 'c, 'c, 'c, 'c, '
                                                  //| d, 'd, 'd)
  
  // Q16 - Drop every Nth element from a list.
  def drop[T](i: Int, ls: List[T]): List[T] = {
    def drop0[T](j: Int, ls: List[T]): List[T] = ls match {
      case Nil => Nil
      case x :: xs => if (j == 1) drop0(i, xs) else x :: drop0(j - 1, xs)
    }
    if (i > 0) drop0(i, ls) else ls
  }                                               //> drop: [T](i: Int, ls: List[T])List[T]
  drop(2, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
                                                  //> res5: List[Symbol] = List('a, 'c, 'e, 'g, 'i, 'k)

  // official solution - using tail recursive.
  def dropTailRecursive[A](n: Int, ls: List[A]): List[A] = {
    def dropR(c: Int, curList: List[A], result: List[A]): List[A] = (c, curList) match {
      case (_, Nil)       => result.reverse
      case (1, _ :: tail) => dropR(n, tail, result)
      case (_, h :: tail) => dropR(c - 1, tail, h :: result)
    }
    dropR(n, ls, Nil)
  }                                               //> dropTailRecursive: [A](n: Int, ls: List[A])List[A]
  
  // official solution - using 'zipWithIndex' - beautiful
  def dropFunctional[A](n: Int, ls: List[A]): List[A] =
    ls.zipWithIndex filter { v => (v._2 + 1) % n != 0 } map { _._1 }
                                                  //> dropFunctional: [A](n: Int, ls: List[A])List[A]
    
    
  // Q17 - Split a list into two parts.
  def split[T](i: Int, ls: List[T]): (List[T], List[T]) = {
  	(ls.take(i), ls.drop(i)) // built-in functions take care of index overflow and Nil cases
  }                                               //> split: [T](i: Int, ls: List[T])(List[T], List[T])
  split(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
                                                  //> res6: (List[Symbol], List[Symbol]) = (List('a, 'b, 'c),List('d, 'e, 'f, 'g,
                                                  //|  'h, 'i, 'j, 'k))
  
  // official solution - builtin function
  def splitBuiltin[A](n: Int, ls: List[A]): (List[A], List[A]) = ls.splitAt(n)
                                                  //> splitBuiltin: [A](n: Int, ls: List[A])(List[A], List[A])

	// Q18 - Extract a slice from a list.
	def slice[A](i: Int, j: Int, ls: List[A]): List[A] = {
		ls.slice(i,j)
	}                                         //> slice: [A](i: Int, j: Int, ls: List[A])List[A]
	slice(3, 7, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
                                                  //> res7: List[Symbol] = List('d, 'e, 'f, 'g)
	
	// official solution - functional.
  def sliceFunctional[A](s: Int, e: Int, ls: List[A]): List[A] =
    ls drop s take (e - (s max 0))                //> sliceFunctional: [A](s: Int, e: Int, ls: List[A])List[A]
    
  // Q19 - Rotate a list N places to the left.
  def rotate[A](i: Int, ls: List[A]): List[A] = {
  	if (i > 0) ls.drop(i) ::: ls.take(i)
  	else {
  		val j = i + ls.length
  		ls.drop(j) ::: ls.take(j)
  	}
  }                                               //> rotate: [A](i: Int, ls: List[A])List[A]
  rotate(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
                                                  //> res8: List[Symbol] = List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k, 'a, 'b, 'c)
  rotate(-2, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
                                                  //> res9: List[Symbol] = List('j, 'k, 'a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i)
	// official solution handles overflow by taking mod (%)
	
	// Q20 - Remove the Kth element from a list.
	def removeAt[A](i: Int, ls: List[A]): (List[A], Any) = {
		val (a, b) = ls splitAt i
		b match {
			case x :: xs => (a ::: xs, x)
			case Nil => (a, Nil)
		}
	}                                         //> removeAt: [A](i: Int, ls: List[A])(List[A], Any)
	removeAt(1, List('a, 'b, 'c, 'd))         //> res10: (List[Symbol], Any) = (List('a, 'c, 'd),'b)
	
	// official solution - a bit better
	def removeAtOfficial[A](n: Int, ls: List[A]): (List[A], A) = ls.splitAt(n) match {
    case (Nil, _) if n < 0 => throw new NoSuchElementException
    case (pre, e :: post)  => (pre ::: post, e)
    case (pre, Nil)        => throw new NoSuchElementException
  }                                               //> removeAtOfficial: [A](n: Int, ls: List[A])(List[A], A)

	// Q21 - Insert an element at a given position into a list.
	def insertAt[A](el: A, n: Int, ls: List[A]): List[A] = ls splitAt(n) match {
    case (Nil, _) if n < 0 => throw new NoSuchElementException
    case (pre, post)  => (pre ::: el :: post)
	}                                         //> insertAt: [A](el: A, n: Int, ls: List[A])List[A]
	insertAt('new, 1, List('a, 'b, 'c, 'd))   //> res11: List[Symbol] = List('a, 'new, 'b, 'c, 'd)

	// Q22 - Create a list containing all integers within a given range.
	def range(i: Int, j: Int): List[Int] = {
		List.range(i, j + 1)
	}                                         //> range: (i: Int, j: Int)List[Int]
	range(4, 9)                               //> res12: List[Int] = List(4, 5, 6, 7, 8, 9)
}