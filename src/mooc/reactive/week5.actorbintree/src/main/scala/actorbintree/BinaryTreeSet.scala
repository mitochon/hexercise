/**
 * Copyright (C) 2009-2013 Typesafe Inc. <http://www.typesafe.com>
 */
package actorbintree

import akka.actor._
import scala.collection.immutable.Queue

object BinaryTreeSet {

  trait Operation {
    def requester: ActorRef
    def id: Int
    def elem: Int
  }

  trait OperationReply {
    def id: Int
  }

  /** Request with identifier `id` to insert an element `elem` into the tree.
    * The actor at reference `requester` should be notified when this operation
    * is completed.
    */
  case class Insert(requester: ActorRef, id: Int, elem: Int) extends Operation

  /** Request with identifier `id` to check whether an element `elem` is present
    * in the tree. The actor at reference `requester` should be notified when
    * this operation is completed.
    */
  case class Contains(requester: ActorRef, id: Int, elem: Int) extends Operation

  /** Request with identifier `id` to remove the element `elem` from the tree.
    * The actor at reference `requester` should be notified when this operation
    * is completed.
    */
  case class Remove(requester: ActorRef, id: Int, elem: Int) extends Operation

  /** Request to perform garbage collection*/
  case object GC

  /** Holds the answer to the Contains request with identifier `id`.
    * `result` is true if and only if the element is present in the tree.
    */
  case class ContainsResult(id: Int, result: Boolean) extends OperationReply
  
  /** Message to signal successful completion of an insert or remove operation. */
  case class OperationFinished(id: Int) extends OperationReply

}


class BinaryTreeSet extends Actor {
  import BinaryTreeSet._
  import BinaryTreeNode._

  def createRoot: ActorRef = context.actorOf(BinaryTreeNode.props(0, initiallyRemoved = true))

  var root = createRoot

  // optional
  var pendingQueue = Queue.empty[Operation]

  // optional
  def receive = normal
  
  // optional
  /** Accepts `Operation` and `GC` messages. */
  val normal: Receive = {
    case op: Operation => root ! op
    case GC => 
      val newRoot = createRoot
      root ! CopyTo(newRoot)
      context.become(garbageCollecting(newRoot))
  }

  // optional
  /** Handles messages while garbage collection is performed.
    * `newRoot` is the root of the new binary tree where we want to copy
    * all non-removed elements into.
    */
  def garbageCollecting(newRoot: ActorRef): Receive = {
    case op: Operation =>
      pendingQueue = pendingQueue.enqueue(op)
    case CopyFinished =>
      pendingQueue foreach (newRoot ! _)
      pendingQueue = Queue.empty[Operation]
      root = newRoot
      context.become(normal)
    case GC =>
      // do nothing
  }
}

object BinaryTreeNode {
  trait Position

  case object Left extends Position
  case object Right extends Position

  case class CopyTo(treeNode: ActorRef)
  case object CopyFinished

  def props(elem: Int, initiallyRemoved: Boolean) = Props(classOf[BinaryTreeNode],  elem, initiallyRemoved)
}

class BinaryTreeNode(val elem: Int, initiallyRemoved: Boolean) extends Actor {
  import BinaryTreeNode._
  import BinaryTreeSet._

  var subtrees = Map[Position, ActorRef]()
  var removed = initiallyRemoved

  // optional
  def receive = normal

  // optional
  /** Handles `Operation` messages and `CopyTo` requests. */
  val normal: Receive = {
    
    case Contains(r, id, e) =>
      if (e == elem) r ! ContainsResult(id, !removed) else {
        pickChild(e) match {
          case Some(n) => n ! Contains(r, id, e)
          case None => r ! ContainsResult(id, false)
        }
      }

    case Insert(r, id, e) =>
      if (e == elem) {
        removed = false
        r ! OperationFinished(id) 
      } else {
        pickChild(e) match {
          case Some(n) => n ! Insert(r, id, e)
          case None => 
            subtrees += (pickSide(e) -> createNode(e))
            r ! OperationFinished(id)
        }
      }
      
    case Remove(r, id, e) => 
      if (e == elem) {
        removed = true
	    r! OperationFinished(id)
      } else {
        pickChild(e) match {
          case Some(n) => n ! Remove(r, id, e)
          case None => r ! OperationFinished(id)
        }
      }
      
    case CopyTo(n) =>
      val children = subtrees.values.toSet
      if (removed && children.isEmpty) stop()
      else {
        if (!removed) n ! Insert(self, -1 * elem, elem)
        children foreach (_ ! CopyTo(n))
        context.become(copying(children, removed))
      }
  }
  
  // optional
  /** `expected` is the set of ActorRefs whose replies we are waiting for,
    * `insertConfirmed` tracks whether the copy of this node to the new tree has been confirmed.
    */
  def copying(expected: Set[ActorRef], insertConfirmed: Boolean): Receive = {
    case CopyFinished => 
      val s = expected - sender
      if (checkDone(s, insertConfirmed)) stop()
      else context.become(copying(s, insertConfirmed))
      
    case OperationFinished(_) =>
      if (checkDone(expected, true)) stop() 
      else context.become(copying(expected, true))
  }

  def pickChild(e: Int): Option[ActorRef] = {
    subtrees.get(pickSide(e))
  }

  def pickSide(e: Int): Position = {
    if (e < elem) Left else Right
  }

  def createNode(e: Int): ActorRef = {
    context.actorOf(BinaryTreeNode.props(e, false), name = "node-" + e)
  }
  
  def checkDone(s: Set[ActorRef], confirmed: Boolean) = {
    s.isEmpty && confirmed
  }
  
  def stop() = {
    context.parent ! CopyFinished
    context.stop(self)
  }
}
