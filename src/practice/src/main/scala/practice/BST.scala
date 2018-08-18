package practice

import java.util.Stack

/**
 * Binary search tree
 */
object BST {

  case class Node(i: Int, left: Option[Node] = None, right: Option[Node] = None)

  /**
   * Traverse depth first using only a stack, printing
   */
  def traverseDepthFirst(n: Node): Unit = {

    var stack = new Stack[Node]()
    var current = n

    while (current != null) {

      if (n.left.isDefined) {
        stack.push(current)
        current = n.left.get
      } else {
        println(n.i)

        if (n.right.isDefined) {
          current = n.right.get
        } else if (!stack.isEmpty()) {
          current = stack.pop
        } else {
          current = null
        }
      }
    }
  }

  /**
   * Checks if the tree is ordered correctly
   */
  def validate(n: Node): Boolean = {

    def check(node: Node, min: Option[Int] = None, max: Option[Int] = None): Boolean = {
      min.forall(node.i >= _) && max.forall(node.i < _) &&
        node.left.forall(check(_, min, Some(node.i))) &&
        node.right.forall(check(_, Some(node.i), max))
    }

    check(n)
  }
}
