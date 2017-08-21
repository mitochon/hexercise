package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {
  
  // helper function to count heap size
  def getSize(h: H): Int = {
    if (isEmpty(h)) 0 else {
      1 + getSize(deleteMin(h))
    }
  }
  
  // Sanity check
  property("checkEmpty") = isEmpty(empty)
  
  // Sample method / given
  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  // From HW hint
  // If you insert any two elements into an empty heap, 
  // finding the minimum of the resulting heap should get the smallest of the two elements back.
  property("min2") = forAll { (a: Int, b: Int) =>
    val h = insert(b, insert(a, empty))
    findMin(h) == math.min(a, b)
  }
  
  // verify the 'correct' min is deleted
  property("min3") = forAll { (a: Int, b: Int) =>
    val h = insert(b, insert(a, empty))
    findMin(deleteMin(h)) == math.max(a, b)
  }
  
  // verify the size of heap after the min is deleted
  property("min4") = forAll { h: H =>
    if (isEmpty(h)) true else {
      getSize(h) == 1 + getSize(deleteMin(h)) 
    }
  }
  
  // From HW hint
  // If you insert an element into an empty heap, 
  // then delete the minimum, the resulting heap should be empty.
  property("insertDelete") = forAll { a: Int =>
    isEmpty(deleteMin(insert(a, empty)))
  }

  // Sample method / given
  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }
  
  // From HW hint
  // Finding a minimum of the melding of any two heaps should return a minimum of one or the other.
  property("minMeld") = forAll { (h1: H, h2: H) =>
    if (!isEmpty(h1) && !isEmpty(h2)) {
      math.min(findMin(h1), findMin(h2)) == findMin(meld(h1, h2))
    } else if (!isEmpty(h1)) {
      findMin(h1) == findMin(meld(h1, h2))
    } else if (!isEmpty(h2)) {
      findMin(h2) == findMin(meld(h1, h2))
    } else { // both heaps are empty
      true
    }
  }
  
  // Check heap size after meld
  property("sizeMeld") = forAll { (h1: H, h2: H) =>
    getSize(meld(h1, h2)) == getSize(h1) + getSize(h2) 
  }
  
  // Check heap integrity after meld
  property("swapMeld") = forAll { (h1: H, h2: H) =>
    def isEqualHeap(h1: H, h2: H): Boolean = {
      if (!isEmpty(h1) && !isEmpty(h2)) {
    	  findMin(h1) == findMin(h2) && isEqualHeap(deleteMin(h1), deleteMin(h2))
      } else { 
        (isEmpty(h1) && isEmpty(h2))
      }
    }
    def orderHeap(h1: H, h2: H) = {
      if (isEmpty(h1) && !(isEmpty(h2))) (h2, h1) else (h1, h2)
    }
    
    val (firstHeap, secondHeap) = orderHeap(h1, h2)
    
    if (isEmpty(firstHeap)) true else {
      isEqualHeap(meld(firstHeap, secondHeap), 
          meld(insert(findMin(firstHeap), secondHeap), 
              deleteMin(firstHeap)))      
    }
  }
  
  // Given any heap, you should get a sorted sequence of elements when continually finding and deleting minima. 
  // (Hint: recursion and helper functions are your friends.)
  property("checkHeap") = forAll { h: H =>
    def isHeapOrdered(h: H): Boolean = {
      if (isEmpty(h)) true else {
        val currMin = findMin(h)
        val nextHeap = deleteMin(h)
        if (isEmpty(nextHeap)) true else {
          (ord.lteq(currMin, findMin(nextHeap)) && isHeapOrdered(nextHeap))
        }
      }
    }
    isHeapOrdered(h)
  }
  
  // First method that needs to be implemented
  lazy val genHeap: Gen[H] = for {
    e <- arbitrary[Int]
    h <- oneOf(value(empty), genHeap)
  } yield insert(e, h)
  
  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  // Sample generator / given
  lazy val genMap: Gen[Map[Int, Int]] = for {
    k <- arbitrary[Int]
    v <- arbitrary[Int]
    m <- oneOf(value(Map.empty[Int, Int]), genMap)
  } yield m.updated(k, v)
}
