package kvstore

import scala.concurrent.duration.DurationInt

import akka.actor.Actor
import akka.actor.ReceiveTimeout

class AckGetter extends Actor {
  
  context.setReceiveTimeout(1.seconds)

  def receive = {
    // send replicate to the set of replicas
    // persist
    case ReceiveTimeout =>
      // context.parent ! send failure 
      context.stop(self)
  }

}