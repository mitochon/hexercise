abstract sealed class Action
case class Atom(atom: Unit => Action) extends Action {
  override def toString() = "atom"
}
case class Fork(a1: Action, a2: Action) extends Action {
  override def toString = s"fork ${a1 toString} ${a2 toString}"
}
case class Stop() extends Action {
  override def toString = "stop"
}

// Note: The ones marked '/* */' are the original exercises

class Concurrent[A](val func: (A => Action) => Action) {

  import Concurrent.roundRobin

  def andThen[B](after: Concurrent[B]): Concurrent[B] = flatMap(_ => after)

  /* */
  def action(): Action = func { x: A => Stop() }
  
  /* */
  def fork(): Concurrent[Unit] = Concurrent {
    (cont: Unit => Action) => Fork(action(), cont())
  }
  
  /* */
  def flatMap[B](mapper: A => Concurrent[B]): Concurrent[B] = Concurrent {
    (cont: B => Action) => func { x: A => mapper(x).func(cont) }
  }

  def run(): () => Unit = roundRobin(List[Action](action))
}

object Concurrent {
  def apply[A](func: (A => Action) => Action) = new Concurrent[A](func)
  def of[A](a: A) = new Concurrent((cont: A => Action) => cont(a))
  
  /* */
  def stop[A](): Concurrent[A] = Concurrent { 
    (cont: A => Action) => Stop() 
  }

  /* */
  def atom[A](ioA: Unit => A): Concurrent[A] = Concurrent {
    (cont: A => Action) => Atom { x: Unit => cont( ioA (x) ) }
  }  

  /* */
  def par[A](c1: Concurrent[A], c2: Concurrent[A]): Concurrent[A] = Concurrent {
    (cont: A => Action) => Fork (c1.action(), c2.action())
  }
  
  /* */
  private def roundRobin(list: List[Action]): () => Unit = list match {
    case Nil => () => Unit
    case head :: tail => head match {
    	case Atom(x) 		=> roundRobin(tail ::: List(x()))
    	case Fork(f1, f2) 	=> roundRobin(f1 :: f2 :: tail)
    	case Stop()			=> roundRobin(tail)
    }    
  }
}
