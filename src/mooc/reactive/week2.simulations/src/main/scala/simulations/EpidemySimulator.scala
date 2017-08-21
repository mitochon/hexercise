package simulations

import math.random

class EpidemySimulator extends Simulator {

  def randomBelow(i: Int) = (random * i).toInt

  protected[simulations] object SimConfig {
    val population: Int = 300
    val roomRows: Int = 8
    val roomColumns: Int = 8

    // to complete: additional parameters of simulation
    val prevalenceRate = 1.0
    val transmissionRate = 40.0
    val sickToDeadRate = 25.0
    val maxWaitDays = 5
  }

  import SimConfig._

  // to complete: construct list of persons
  val persons: List[Person] = ((1 to population) map (x => new Person(x))).toList 

  // initialize
  persons take (prevalenceRate / 100.0 * population).toInt map(_.infect())
  persons map (_.action())
  
  // return true if any person in the same 'room' is infected
  def infectious(row: Int, col: Int): Boolean = {
    persons exists (p => p.row == row && p.col == col && p.infected)
  }

  // return true if any person in the same 'room' is sick or dead
  def visiblyInfectious(row: Int, col: Int): Boolean = {
    persons exists (p => p.row == row && p.col == col && (p.sick || p.dead))
  }

  class Person (val id: Int) {
    var infected = false
    var sick = false
    var immune = false
    var dead = false

    // demonstrates random number generation
    var row: Int = randomBelow(roomRows)
    var col: Int = randomBelow(roomColumns)

    //
    // to complete with simulation logic
    //
    
    def action(): Unit = {

      def move() = {
        afterDelay(randomBelow(maxWaitDays) + 1) {
          
          // After each move (and also after the beginning of the simulation),
          // a person moves to one of their neighbouring rooms (left, right, up, down)
          // within the next 5 days (with equally distributed probability).

          val (toRow, toCol) = moveAdjacent()
          
          // A person avoids rooms with sick or dead (visibly infectious) people.
          // This means that if a person is surrounded by visibly infectious people, he does not change position; 
          // however, he might change position the next time he tries to move 
          // (for example, if a visibly infectious person moved out of one of the neighbouring rooms or became immune).

          if (!dead && !visiblyInfectious(toRow, toCol)) {
            // TODO remove println(s"Moving from ($row,$col) to ($toRow,$toCol) => (infected $infected, sick $sick, immune $immune, dead $dead)")
            row = toRow; col = toCol

            // When a person moves into a room with an infectious person he might get infected according to the transmissibility rate, 
            // unless the person is already infected or immune. 
            // A person cannot get infected between moves (this is slightly unrealistic, but will simplify your implementation).

            if (!infected && infectious(row, col) && randomBelow(100) < transmissionRate) {
              // TODO remove println("infecting person " + id)
              infect()
            }
          }
            
          // loop back            
          action
        }
      }

      def moveAdjacent(): (Int, Int) = {
        def leftRight() = {
          (row, shift(col, roomColumns - 1, (randomBelow(2) == 1)))
        }
        def upDown() = {
          (shift(row, roomRows - 1, (randomBelow(2) == 1)), col)
        }
        def shift(i: Int, max: Int, add: Boolean): Int = {
          if (add) {
            if (i < max) i + 1 else 0
          } else {
            if (i > 0) i - 1 else max
          }
        }
        if (randomBelow(2) == 1) upDown() else leftRight()
      }

      // Therefore, a person moves repeatedly until he is dead (discussed below), 
      // and the waiting time between two moves varies from 1 to 5 days (randomly decided by you each time). 
      // Note that the first row is considered to be a neighbour of row eight (and vice versa); 
      // analogously, the first column is a neighbour of column eight (and vice versa).

      if (!dead) move()
    }
        
    // When a person becomes infected, he does not immediately get sick, but enters a phase of incubation in which he is infectious but not sick.
    // After 6 days of becoming infected, a person becomes sick and is therefore visibly infectious.
    // After 14 days of becoming infected, a person dies with a probability of 25%. Dead people do not move, but stay visibly infectious.
    // After 16 days of becoming infected, a person becomes immune. 
    // He is no longer visibly infectious, but remains infectious. An immune person cannot get infected.
    // After 18 days of becoming infected, a person turns healthy. 
    // He is now in the same state as he was before his infection, which means that he can get infected again

    def infect() = {
      def mayBecomeSick() =
        afterDelay(6) { 
    	  sick = true 
    	}
      
      def mayBecomeDead() =
        afterDelay(14) {
          if (randomBelow(100) < sickToDeadRate) { 
            dead = true // stays 'sick'
          }
        }
      
      def mayBecomeImmune() =
        afterDelay(16) { 
    	  if (!dead) {
    	    immune = true
    	    sick = false
    	  }
    	}      
      
      def mayBecomeHealthy() = 
        afterDelay(18) {
          if (!dead) {
            immune = false
            infected = false
          }
        }

      infected = true
      mayBecomeSick()
      mayBecomeDead()
      mayBecomeImmune()
      mayBecomeHealthy()
    }    
  }
}
