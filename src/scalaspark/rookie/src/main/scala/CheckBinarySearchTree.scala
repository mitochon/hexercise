/**
 * Question:
 * Given a BST, make a function that checks if the BST is properly formed, 
 * i.e. for each node, left tree <= value <= right tree.
 * Also design a test case to check the code.
 * 
 * Answer:
 * See checkTree() below and the accompanying test suite.
 */
object CheckBinarySearchTree extends App {
  
  /** Define a BST */
  sealed trait Tree {
    def add(e: Int): Tree
  }
  case class Node(v: Int, left: Tree, right: Tree) extends Tree {
    def add(e: Int): Tree = 
      if (e <= v) Node(v, left.add(e), right)
      else Node(v, left, right.add(e))
  }  
  case object Stump extends Tree {
    def add(e: Int): Tree = Node(e, Stump, Stump)
  }
  
  
  /** Tools for manipulating the BST */
  
  // returns true if the Tree is a binary search tree
  def checkTree(t: Tree): Boolean = {
    val l = flatten(t)
    if (l.isEmpty) true else l.zip(l.tail).forall(e => e._1 <= e._2)
  }
  
  // 'walks' the Tree and return the elements in order
  def flatten(t: Tree): List[Int] = {
    import scala.annotation.tailrec
    @tailrec def flattenIter(acc: List[Int], queue: List[Tree]): List[Int] = queue match {
      case Nil => acc
      case Stump :: xs => flattenIter(acc, xs)
      case Node(v, Stump, Stump) :: xs => flattenIter(v :: acc, xs)
      case Node(v, l, Stump) :: xs => flattenIter(acc, l :: Node(v, Stump, Stump) :: xs)
      case Node(v, Stump, r) :: xs => flattenIter(v :: acc, r :: xs)
      case Node(v, l, r) :: xs => flattenIter(acc, l :: Node(v, Stump, Stump) :: r :: xs)
    }
    flattenIter(List(), List(t)).reverse
  }
    
  // creates a binary search tree from a list of integers
  def build(l: List[Int]): Tree = {
    def buildIter(t: Tree, remaining: List[Int]): Tree = remaining match {
      case Nil => t
      case x :: xs => buildIter(t.add(x), xs)
    }
    buildIter(Stump, l)    
  }
  
  // creates a 'bad' binary search tree from a list of integers
  def buildCorrupt(l: List[Int]): Tree = {
    def corrupt(t: Tree, b: Boolean): Tree = t match {
        case Node(v, l, r) => if (b) Node(v, r, l) else Node(v, l, r)
        case Stump => Stump
      }
    def buildIter(t: Tree, count: Int, c: Int, l: List[Int]): Tree = l match {
      case Nil => t
      case x :: xs => buildIter(corrupt(t.add(x), count == c), count + 1, c, xs)
    }
    // choose a random element to corrupt
    val c = 1 + util.Random.nextInt(math.max(1, l.length - 1)) 
    buildIter(Stump, 0, c, l)
  }
  
  
  /** Test Suite */
  
  import org.scalacheck.Prop._

  val testCheckGoodTree = forAll { l: List[Int] =>
    checkTree(build(l)) 					// uncorrupt tree should succeed
  }

  val testCheckCorruptTree = forAll { l: List[Int] =>
    val uniques = l.distinct				// if members are non-unique, list may fail to be corrupted
    if (uniques.size < 2) true 				// cannot generate corrupt tree with sample of 0 or 1
    else !checkTree(buildCorrupt(uniques)) 	// corrupt tree should fail 			
  }

  // run tests
  testCheckGoodTree.check
  testCheckCorruptTree.check
  
  /** TODO test 'build' , 'buildCorrupt' and 'flatten' */

}