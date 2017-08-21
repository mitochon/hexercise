object ninetynine3 {

	// Q23 - Extract a given number of randomly selected elements from a list.
	// from Q20
	def removeAtOfficial[A](n: Int, ls: List[A]): (List[A], A) = ls.splitAt(n) match {
    case (Nil, _) if n < 0 => throw new NoSuchElementException
    case (pre, e :: post)  => (pre ::: post, e)
    case (pre, Nil)        => throw new NoSuchElementException
  }                                               //> removeAtOfficial: [A](n: Int, ls: List[A])(List[A], A)
  
	def randomSelect[A](n: Int, ls: List[A]): List[A] = {
		val r = new util.Random
    def random0[A](n: Int, ls: List[A], acc: List[A]): List[A] = {
      if (n < 1) acc else {
        val (xs, elm) = removeAtOfficial(r.nextInt(ls.length), ls)
        random0(n - 1, xs, elm :: acc)
      }
    }
		random0(n, ls, List())
	}                                         //> randomSelect: [A](n: Int, ls: List[A])List[A]
	randomSelect(3, List('a, 'b, 'c, 'd, 'f, 'g, 'h))
                                                  //> res0: List[Symbol] = List('c, 'a, 'f)
	
	// Q24 - Lotto: Draw N different random numbers from the set 1..M.
	def lotto(n: Int, r: Int) = randomSelect(6, List.range(1, r + 1))
                                                  //> lotto: (n: Int, r: Int)List[Int]
	lotto(6, 49)                              //> res1: List[Int] = List(26, 31, 9, 37, 22, 13)
	
	// Q25 - Generate a random permutation of the elements of a list.
	def randomPermute[A](ls: List[A]): List[A] = randomSelect(ls.length, ls)
                                                  //> randomPermute: [A](ls: List[A])List[A]
	randomPermute(List('a, 'b, 'c, 'd, 'e, 'f))
                                                  //> res2: List[Symbol] = List('a, 'd, 'b, 'c, 'e, 'f)

  // Q26 - Generate the combinations of K distinct objects chosen from the N elements of a list.
  def combinations[A](i: Int, ls: List[A]): List[List[A]] = {
    def shift(ls: List[Int], i: Int, rotate: Boolean): List[Int] = ls.splitAt(i) match {
      case (h, t :: Nil) => h ::: List(1)
      case (h, t1 :: t2 :: ts) => {
        if (rotate) h ::: t2 :: t1 :: ts.reverse
        else h ::: t2 :: t1 :: ts
      }
    }
    def findNext(ls: List[Int], offset: Int): List[Int] = {
      val (i, j, k) = (ls.lastIndexOf(0), ls.lastIndexOf(1), ls.indexOf(1))
      if (i == -1 || i < k) ls
      else if (i > j) shift(ls, j, false)
      else {
        if (i < ls.length - offset)
          findNext(ls, offset + 1)
        else shift(ls, ls.lastIndexOf(1, i), true)
      }
    }
    def generateList(ls: List[A], prev: List[Int], acc: List[List[A]]): List[List[A]] = {
      val curr = findNext(prev, 1)
      val elm = ls zip prev filter (x => x._2 == 1) map (_._1)
      if (prev == curr) (elm :: acc).reverse
      else generateList(ls, curr,  elm :: acc)
    }
    if (ls.isEmpty || i > ls.length) List()
    else generateList(ls, List.fill(i)(1) ::: List.fill(ls.length - i)(0), List[List[A]]())
	}                                         //> combinations: [A](i: Int, ls: List[A])List[List[A]]
	combinations(5, List('a, 'b, 'c, 'd, 'e, 'f, 'g))
                                                  //> res3: List[List[Symbol]] = List(List('a, 'b, 'c, 'd, 'e), List('a, 'b, 'c, 
                                                  //| 'd, 'f), List('a, 'b, 'c, 'd, 'g), List('a, 'b, 'c, 'e, 'f), List('a, 'b, '
                                                  //| c, 'e, 'g), List('a, 'b, 'c, 'f, 'g), List('a, 'b, 'd, 'e, 'f), List('a, 'b
                                                  //| , 'd, 'e, 'g), List('a, 'b, 'd, 'f, 'g), List('a, 'b, 'e, 'f, 'g), List('a,
                                                  //|  'c, 'd, 'e, 'f), List('a, 'c, 'd, 'e, 'g), List('a, 'c, 'd, 'f, 'g), List(
                                                  //| 'a, 'c, 'e, 'f, 'g), List('a, 'd, 'e, 'f, 'g), List('b, 'c, 'd, 'e, 'f), Li
                                                  //| st('b, 'c, 'd, 'e, 'g), List('b, 'c, 'd, 'f, 'g), List('b, 'c, 'e, 'f, 'g),
                                                  //|  List('b, 'd, 'e, 'f, 'g), List('c, 'd, 'e, 'f, 'g))

	// official solution - recursive - very complicated
  def flatMapSublists[A, B](ls: List[A])(f: (List[A]) => List[B]): List[B] =
    ls match {
      case Nil => Nil
      case sublist @ (_ :: tail) => f(sublist) ::: flatMapSublists(tail)(f)
    }                                             //> flatMapSublists: [A, B](ls: List[A])(f: List[A] => List[B])List[B]

  def combinationsFunctional[A](n: Int, ls: List[A]): List[List[A]] =
    if (n == 0) List(Nil)
    else flatMapSublists(ls) { sl =>
      combinationsFunctional(n - 1, sl.tail) map { sl.head :: _ }
    }                                             //> combinationsFunctional: [A](n: Int, ls: List[A])List[List[A]]
 	combinationsFunctional(5, List('a, 'b, 'c, 'd, 'e, 'f, 'g))
                                                  //> res4: List[List[Symbol]] = List(List('a, 'b, 'c, 'd, 'e), List('a, 'b, 'c, 
                                                  //| 'd, 'f), List('a, 'b, 'c, 'd, 'g), List('a, 'b, 'c, 'e, 'f), List('a, 'b, '
                                                  //| c, 'e, 'g), List('a, 'b, 'c, 'f, 'g), List('a, 'b, 'd, 'e, 'f), List('a, 'b
                                                  //| , 'd, 'e, 'g), List('a, 'b, 'd, 'f, 'g), List('a, 'b, 'e, 'f, 'g), List('a,
                                                  //|  'c, 'd, 'e, 'f), List('a, 'c, 'd, 'e, 'g), List('a, 'c, 'd, 'f, 'g), List(
                                                  //| 'a, 'c, 'e, 'f, 'g), List('a, 'd, 'e, 'f, 'g), List('b, 'c, 'd, 'e, 'f), Li
                                                  //| st('b, 'c, 'd, 'e, 'g), List('b, 'c, 'd, 'f, 'g), List('b, 'c, 'e, 'f, 'g),
                                                  //|  List('b, 'd, 'e, 'f, 'g), List('c, 'd, 'e, 'f, 'g))
 
}