package kvstore

import akka.actor.{ OneForOneStrategy, Props, ActorRef, Actor }
import kvstore.Arbiter._
import scala.collection.immutable.Queue
import akka.actor.SupervisorStrategy.Restart
import scala.annotation.tailrec
import akka.pattern.{ ask, pipe }
import akka.actor.Terminated
import scala.concurrent.duration._
import akka.actor.PoisonPill
import akka.actor.OneForOneStrategy
import akka.actor.SupervisorStrategy
import akka.util.Timeout
import akka.actor.Cancellable

object Replica {
  sealed trait Operation {
    def key: String
    def id: Long
  }
  case class Insert(key: String, value: String, id: Long) extends Operation
  case class Remove(key: String, id: Long) extends Operation
  case class Get(key: String, id: Long) extends Operation

  sealed trait OperationReply
  case class OperationAck(id: Long) extends OperationReply
  case class OperationFailed(id: Long) extends OperationReply
  case class GetResult(key: String, valueOption: Option[String], id: Long) extends OperationReply

  def props(arbiter: ActorRef, persistenceProps: Props): Props = Props(new Replica(arbiter, persistenceProps))
}

class Replica(val arbiter: ActorRef, persistenceProps: Props) extends Actor {
  import Replica._
  import Replicator._
  import Persistence._
  import context.dispatcher

  /*
   * The contents of this actor is just a suggestion, you can implement it in any way you like.
   */
  
  var kv = Map.empty[String, String]
  // a map from secondary replicas to replicators
  var secondaries = Map.empty[ActorRef, ActorRef]
  // the current set of replicators
  var replicators = Set.empty[ActorRef]
  // a map of pending persist requests
  var persistAcks = Map.empty[Long, (ActorRef, Cancellable)]
  // a map of pending operation acks
  var pendingAcks = Map.empty[Long, (Boolean, Set[ActorRef], ActorRef, Cancellable, Cancellable)]

  /* call arbiter to Join */
  arbiter ! Join
  
  def receive = {
    case JoinedPrimary   => context.become(leader)
    case JoinedSecondary => context.become(replica)
  }

  val persistor = context.actorOf(persistenceProps, "persistor")
  
  override val supervisorStrategy = OneForOneStrategy(maxNrOfRetries = 5) {
    case _: Exception => SupervisorStrategy.Restart
  }
  
  /* TODO Behavior for  the leader role. */
  val leader: Receive = {
    
    case Insert(k, v, id) => 
      kv = kv + (k -> v)
      getAck(id, k, Some(v), sender)
      
    case Remove(k, id) => 
      kv = kv - k
      getAck(id, k, None, sender)
    
    case Get(k, id) =>
      val v = kv get k
      sender ! GetResult(k, v, id)
    
    case Replicas(rSet) =>
      var updMap = Map.empty[ActorRef, ActorRef]
      rSet foreach (r => {
        if (r != self) {
          secondaries get r match {
            case Some(v) => updMap += (r -> v); secondaries -= r
            case None => 
              val rpltr = context.actorOf(Replicator.props(r))
              updMap += (r -> rpltr)
              kv foreach (entry => rpltr ! Replicate (entry._1, Some(entry._2), 0))
          }
        }
      })
      //secondaries.values foreach (context.stop(_)) // remove remaining
      secondaries.values foreach ( rpltr => { 
        context.stop(rpltr) // remove remaining
        pendingAcks foreach (kv => {
          pendingAcks = pendingAcks + (kv._1 -> (kv._2._1, kv._2._2 - rpltr, kv._2._3, kv._2._4, kv._2._5))
        })
      })
      // reassign
      secondaries = updMap
      replicators = secondaries.values.toSet
      
    case Persisted(k, id) =>
      pendingAcks get id match {
        case Some((b, r, s, t1, t2)) =>
          pendingAcks = pendingAcks + (id -> (true, r, s, t1, t2))
          if (r.isEmpty)
            sendAck(id, s, t1, t2)
        case None => // do nothing
      }

    case Replicated(k, id) =>
      pendingAcks get id match {
        case Some((b, r, s, t1, t2)) =>
          val remaining = r - sender
          pendingAcks = pendingAcks + (id -> (b, remaining, s, t1, t2))
          if (b && remaining.isEmpty) 
            sendAck(id, s, t1, t2)
        case None => // do nothing
      }
  }
  
  def sendAck(id: Long, s: ActorRef, t1: Cancellable, t2: Cancellable) = {
    t1.cancel
    t2.cancel
    pendingAcks = pendingAcks - id
    s ! OperationAck(id)
  }
  
  def getAck(id: Long, k: String, v: Option[String], r: ActorRef) = {
    broadcastCopy(k, v, id)
    pendingAcks = pendingAcks + (id -> (false, replicators.toSet, r, persist(id, k, v), ackTimer(id, r)))
  }
  
  def ackTimer(id: Long, r: ActorRef): Cancellable = {
    context.system.scheduler.scheduleOnce(1.seconds) {
      pendingAcks get id match {
        case Some((_, _, _, task, _)) => task.cancel
        case None => // do nothing
      }
      pendingAcks = pendingAcks - id
      r ! OperationFailed(id)
    }
  }
  
  def broadcastCopy(k: String, v: Option[String], id: Long) = {
    replicators foreach (_ ! Replicate (k, v, id))
  }
  
  def persist(id: Long, key: String, v: Option[String]): Cancellable = {
    context.system.scheduler.schedule(0.millis, 100.millis) {
      persistor ! Persist(key, v, id)
    }
  }

  var expectedSeq = 0L

  /* TODO Behavior for the replica role. */
  val replica: Receive = {
    
    case Get(k, id) => 
      val v = kv get k
      sender ! GetResult(k, v, id)
      
    case Snapshot(k, v, seq) =>
      if (seq < expectedSeq) {
        sender ! SnapshotAck(k, seq)
      } else if (seq == expectedSeq) {
        v match {
          case Some(s) => kv = kv + (k -> s) // Insert
          case None => kv = kv - k // Remove
        }
        persistAcks = persistAcks + (seq -> (sender, persist(seq, k, v)))

      } // else ignore
      
    case Persisted(k, seq) =>
      persistAcks get seq match {
        case Some((s, c)) => 
          persistAcks = persistAcks - seq
          c.cancel
          expectedSeq += 1
          s ! SnapshotAck(k, seq)
        case None =>
          // do nothing
      }
  }

}
