package week8

object Generators {

  trait Generator[+T] {
    self =>

    def generate: T

    def flatMap[S](f: T => Generator[S]): Generator[S] = new Generator[S] {
      def generate = f(self.generate).generate
    }

    def map[S](f: T => S): Generator[S] = new Generator[S] {
      def generate = f(self.generate)
    }
  }

  def choose(lo: Int, hi: Int)(test: Int => Boolean): Boolean = {
    val value = range(lo, hi).generate
    println("testing with "+value)
    assert(test(value), "test failed for "+value)
    true
  }

  def ForAll[T](test: T => Boolean)(implicit g: Generator[T]): Boolean = {
    val value = g.generate
    println("testing with "+value)
    assert(test(value), "test failed for "+value)
    true
  }

  def property(msg: String, numTimes: Int = 100)(test: => Boolean) = {
    println("testing "+msg)
    for (i <- 0 until numTimes) {
      test
    }
    println("passed "+numTimes+" tests")
  }

  /** Used as an example to show how generators are used. */
  def test[T](numTimes: Int)(g: Generator[T])(test: T => Boolean) {
    for (i <- 0 until numTimes) {
      val value = g.generate
      assert(test(value), "test failed for "+value)
    }
    println("passed "+numTimes+" tests")
  }

  /** Given as an example of a basic generator. */
  implicit def integers: Generator[Int] = new Generator[Int] {
    def generate = scala.util.Random.nextInt()
  }

  /** IMPLEMENT */
  implicit def booleans: Generator[Boolean] = integers.map(_ >= 0)

  /** IMPLEMENT */
  implicit def single[T](x: T): Generator[T] = new Generator[T] {
    def generate = x
  }

  implicit def range(lo: Int, hi: Int): Generator[Int] = new Generator[Int] {
    def generate = scala.util.Random.nextInt(hi - lo) + lo
  }
  /** Given as an example of how to combine 2 generators. */
  implicit def pairs[T, U](implicit t: Generator[T], u: Generator[U]): Generator[(T, U)] = for {
    x <- t
    y <- u
  } yield (x, y)

  /** IMPLEMENT */
  val pairDesugared: Generator[(Int, Int)] = integers flatMap {
    x => integers map { y => (x, y) }
  }

  /** IMPLEMENT */
  def emptyLists[T]: Generator[List[T]] = single(Nil)

  /** IMPLEMENT */
  def nonEmptyLists[T](implicit t: Generator[T]): Generator[List[T]] = for {
    head <- t
    tail <- lists[T]
  } yield head :: tail

  /** IMPLEMENT */
  implicit def lists[T](implicit t: Generator[T]): Generator[List[T]] = for {
    cutoff <- range(0, 4)
    list <- if (cutoff == 0) emptyLists else nonEmptyLists[T]
  } yield list
}