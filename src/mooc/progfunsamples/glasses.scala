package week7

object tryglasses {

	new Pouring(Vector(9, 4)).solutions(6)    //> res0: scala.collection.immutable.Stream[week7.Pouring#Path] = Stream(Fill(0) 
                                                  //| Pour(0,1) Empty(1) Pour(0,1) Empty(1) Pour(0,1) Fill(0) Pour(0,1) --> Vector(
                                                  //| 6, 4), ?)

  new Pouring(Vector(4, 7, 19)).solutions(17)     //> res1: scala.collection.immutable.Stream[week7.Pouring#Path] = Stream(Fill(1)
                                                  //|  Pour(1,2) Fill(1) Pour(1,0) Pour(1,2) Fill(1) Pour(1,2) --> Vector(4, 0, 17
                                                  //| ), ?)
}

class Pouring(capacity: Vector[Int]) {

// States

  type State = Vector[Int]
  val initialState = capacity map (_ => 0)

// Moves

	sealed trait Move {
	  def change(prev: State): State
	}
	case class Empty(glass: Int) extends Move {
	  def change(prev: State) = prev updated (glass, 0)
	}
	case class Fill(glass: Int) extends Move {
	  def change(prev: State) = prev updated (glass, capacity(glass))
	}
	case class Pour(from: Int, to: Int) extends Move {
	  def change(prev: State) = {
	    val amount = prev(from) min (capacity(to) - prev(to))
	    prev updated (from, prev(from) - amount) updated (to, prev(to) + amount)
	  }
	}
	
	val glasses = 0 until capacity.length
	
	val actions =
    { for (glass <- glasses) yield Empty(glass) } ++
    { for (glass <- glasses) yield Fill(glass) } ++
    { for (from <- glasses; to <- glasses if from != to) yield Pour(from, to) }
    
// Paths

  class Path(history: List[Move], val endState: State) {
    def extend(action: Move) = new Path(action :: history, action change endState)
    override def toString = (history.reverse mkString " ") + " --> " + endState
  }
  
  val initialPath = new Path(Nil, initialState)
  
// State exploration

  def from(initial: Set[Path], explored: Set[State]): Stream[Set[Path]] =
    if (initial.isEmpty) Stream.empty
    else {
      val more: Set[Path] = for {
        start <- initial
        next <- actions map start.extend
        if !(explored contains next.endState)
      } yield next
      initial #:: from(more, explored ++ (more map (_.endState)))
    }
    
  lazy val pathSets = from(Set(initialPath), Set(initialState))

  def solutions(target: Int): Stream[Path] =
    for {
      paths <- pathSets
      path <- paths
      if path.endState contains target
    } yield path
}