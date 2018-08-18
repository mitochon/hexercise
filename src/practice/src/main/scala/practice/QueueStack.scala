package practice

import java.util.{ LinkedList, Stack }
import java.util.Queue

/**
 * Implement a queue using two stacks
 */
case class Quu[A]() {

  var stack1 = new Stack[A]();
  var stack2 = new Stack[A]();

  def enq(a: A): Unit = {
    if (stack1.isEmpty()) stack2.push(a)
    else stack1.push(a)
  }

  def deq: A = {
    def unload(a: Stack[A], b: Stack[A]) = {
      while (!a.empty()) b.push(a.pop)
      b.pop
    }
    if (stack1.isEmpty) unload(stack2, stack1)
    else unload(stack1, stack2)
  }
}

/**
 * Implements a stack using two queues
 */
case class Stak[A]() {
  var q1 = new LinkedList[A]();
  var q2 = new LinkedList[A]();

  def push(a: A): Unit = {
    if (q1.isEmpty) q2.add(a)
    else q2.add(a)
  }

  def pop: A = {
    def unload(a: Queue[A], b: Queue[A]) = {
      while (!a.isEmpty()) b.add(a.remove())
      b.remove()
    }
    if (q1.isEmpty) unload(q2, q1)
    else unload(q1, q2)
  }
}