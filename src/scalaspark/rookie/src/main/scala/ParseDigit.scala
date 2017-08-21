/**
 * Question:
 * 
 * If 1 -> 'a', 2 -> 'b', etc, find the size of possibilities for decoding a given string, e.g
 *
 * "11" -> decodes to aa, k. The function should return 2.
 * "111" decodes to aaa, ak, ka. The function should return 3.
 * "31" decodes to ca. The function should return 1
 * "321" decodes to cba, cu. The function should return 2.
 * "101" decodes to ja. The function should return 1.
 * "010" decodes to nothing because a leading zero is not valid. The function should return 0
 * "111100001" decodes to nothing because multiple consecutive zeros is not valid. This function should return 0
 * 
 * Answer:
 * 
 * 2 solutions, one using simple recursion, the other using tail-recursion
 * 
 */

object ParseDigit extends App {
  
  // maps '1' -> 'a', '2' -> 'b', ... , '26' -> 'z'  
  val strMap = ((1 to 26) map (_.toString)) zip ('a' to 'z') toMap
  
  val tests = List("11", "111", "31", "321", "101", "010", "111100001")

  /** First solution using recursion */
  
  def parseRecursive(s: String): Int =
    if (s == null) 0
    else {
      def addCountAt(index: Int, s: String): Int = {
        if (s.length() < index) 0
        else {
          val (left, right) = s splitAt index
          if (!strMap.contains(left)) 0
          else if (right.length == 0) 1
          else parseRecursive(right)
        }
      }
      addCountAt(1, s) + addCountAt(2, s)
    }
  
  tests map (t => println(t + " ==> " + parseRecursive(t)))
  
  
  /** Second solution with tail recursion */
 
  import scala.annotation.tailrec
  
  def parse(s: String): Int = {
    type Element = (List[Char], String)
    val deadend = (Nil, "")
    
    def lookup(e: Element, i: Int): Element = {
      if (i > e._2.length) deadend
      else {
        val (head, tail) = e._2 splitAt i
        strMap get head match {
          case None => deadend
          case Some(n) => (n :: e._1, tail)
        }
      }
    }
    
    @tailrec def expand(ls: List[Element]): List[Element] = {
      if ((ls filter (_._2.length > 0) size) == 0)
        ls // terminate if no more string to process
      else {
        val next = ls.foldLeft[List[Element]](List())(
          (acc, e) => {
            if (e._2.isEmpty) e :: acc
            else lookup(e, 1) :: lookup(e, 2) :: acc
          })
        // for debug --> println(next filterNot (_ == deadend)))
        expand(next filterNot (_ == deadend))
      }
    }
    expand(List((List(), s))).length
  }  
  
  tests map (t => println(t + " @tailrec ==> " + parse(t)))
}