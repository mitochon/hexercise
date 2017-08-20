import common._
import barneshut.conctrees._

package object barneshut {

  class Boundaries {
    var minX = Float.MaxValue

    var minY = Float.MaxValue

    var maxX = Float.MinValue

    var maxY = Float.MinValue

    def width = maxX - minX

    def height = maxY - minY

    def size = math.max(width, height)

    def centerX = minX + width / 2

    def centerY = minY + height / 2

    override def toString = s"Boundaries($minX, $minY, $maxX, $maxY)"
  }

  sealed abstract class Quad {
    def massX: Float

    def massY: Float

    def mass: Float

    def centerX: Float

    def centerY: Float

    def size: Float

    def total: Int

    def insert(b: Body): Quad

    // not needed apparently
    def isWithinBounds(b: Body): Boolean = {
      val leftBound = centerX - (size / 2)
      val rightBound = centerX + (size / 2)
      val upperBound = centerY + (size / 2)
      val lowerBound = centerY - (size / 2)
      leftBound <= b.x && rightBound >= b.x &&
        lowerBound <= b.y && upperBound >= b.y
    }
  }

  case class Empty(centerX: Float, centerY: Float, size: Float) extends Quad {
    def massX: Float = centerX
    def massY: Float = centerY
    def mass: Float = 0
    def total: Int = 0
    // no check if body is within bounds
    def insert(b: Body): Quad = Leaf(centerX, centerY, size, Seq(b))
  }

  case class Fork(
    nw: Quad, ne: Quad, sw: Quad, se: Quad
  ) extends Quad {
    val centerX: Float = (nw.centerX + ne.centerX) / 2
    val centerY: Float = (nw.centerY + sw.centerY) / 2
    val size: Float = nw.size * 2
    val mass: Float = nw.mass + ne.mass + sw.mass + se.mass
    val massX: Float = if (mass == 0) centerX else ((nw.mass * nw.massX) + (ne.mass * ne.massX) + (sw.mass * sw.massX) + (se.mass * se.massX)) / mass
    val massY: Float = if (mass == 0) centerY else ((nw.mass * nw.massY) + (ne.mass * ne.massY) + (sw.mass * sw.massY) + (se.mass * se.massY)) / mass
    val total: Int = nw.total + ne.total + sw.total + se.total

    def insert(b: Body): Fork = {
      (b.x - centerX, b.y - centerY) match {
        case (dX, dY) if (dX < 0 && dY < 0)   => Fork(nw.insert(b), ne, sw, se)
        case (dX, dY) if (dX >= 0 && dY >= 0) => Fork(nw, ne, sw, se.insert(b))
        case (dX, dY) if (dX < 0 && dY >= 0)  => Fork(nw, ne, sw.insert(b), se)
        case (dX, dY) if (dX >= 0 && dY < 0)  => Fork(nw, ne.insert(b), sw, se)
      }
    }
  }

  case class Leaf(centerX: Float, centerY: Float, size: Float, bodies: Seq[Body])
  extends Quad {
    val mass = bodies.map(_.mass).sum
    val massX = bodies.map(b => b.mass * b.x).sum / mass
    val massY = bodies.map(b => b.mass * b.y).sum / mass
    val total: Int = bodies.size
    // If the size of a Leaf is greater than a predefined constant minimumSize, 
    // inserting an additonal body into that Leaf quadtree creates a Fork quadtree with empty children,
    // and adds all the bodies into that Fork (including the new body).
    // Otherwise, inserting creates another Leaf with all the existing bodies and the new one.
    def insert(b: Body): Quad = {
      if (size > minimumSize) {
        val nw = Empty(centerX - size / 4, centerY - size / 4, size / 2)
        val ne = Empty(centerX + size / 4, centerY - size / 4, size / 2)
        val sw = Empty(centerX - size / 4, centerY + size / 4, size / 2)
        val se = Empty(centerX + size / 4, centerY + size / 4, size / 2)
        val fork = Fork(nw, ne, sw, se)
        (b +: bodies).foldLeft(fork)((f, e) => f.insert(e))
      } else {
        Leaf(centerX, centerY, size, b +: bodies)
      }
    }
  }

  def minimumSize = 0.00001f

  def gee: Float = 100.0f

  def delta: Float = 0.01f

  def theta = 0.5f

  def eliminationThreshold = 0.5f

  def force(m1: Float, m2: Float, dist: Float): Float = gee * m1 * m2 / (dist * dist)

  def distance(x0: Float, y0: Float, x1: Float, y1: Float): Float = {
    math.sqrt((x1 - x0) * (x1 - x0) + (y1 - y0) * (y1 - y0)).toFloat
  }

  class Body(val mass: Float, val x: Float, val y: Float, val xspeed: Float, val yspeed: Float) {

    def updated(quad: Quad): Body = {
      var netforcex = 0.0f
      var netforcey = 0.0f

      def addForce(thatMass: Float, thatMassX: Float, thatMassY: Float): Unit = {
        val dist = distance(thatMassX, thatMassY, x, y)
        /* If the distance is smaller than 1f, we enter the realm of close
         * body interactions. Since we do not model them in this simplistic
         * implementation, bodies at extreme proximities get a huge acceleration,
         * and are catapulted from each other's gravitational pull at extreme
         * velocities (something like this:
         * http://en.wikipedia.org/wiki/Interplanetary_spaceflight#Gravitational_slingshot).
         * To decrease the effect of this gravitational slingshot, as a very
         * simple approximation, we ignore gravity at extreme proximities.
         */
        if (dist > 1f) {
          val dforce = force(mass, thatMass, dist)
          val xn = (thatMassX - x) / dist
          val yn = (thatMassY - y) / dist
          val dforcex = dforce * xn
          val dforcey = dforce * yn
          netforcex += dforcex
          netforcey += dforcey
        }
      }

      def traverse(quad: Quad): Unit = (quad: Quad) match {
        case Empty(_, _, _) =>
          // no force
        case Leaf(_, _, _, bodies) =>
          // add force contribution of each body by calling addForce
          bodies.foreach(b => addForce(b.mass, b.x, b.y))
        case Fork(nw, ne, sw, se) =>
          // see if node is far enough from the body,
          // or recursion is needed
          val dist = distance(x, y, quad.centerX, quad.centerY)
          val farEnough = (quad.size / dist) < theta
          if (farEnough) {
            addForce(quad.mass, quad.massX, quad.massY)
          } else {
            traverse(nw)
            traverse(ne)
            traverse(sw)
            traverse(se)
          }
      }

      traverse(quad)

      val nx = x + xspeed * delta
      val ny = y + yspeed * delta
      val nxspeed = xspeed + netforcex / mass * delta
      val nyspeed = yspeed + netforcey / mass * delta

      new Body(mass, nx, ny, nxspeed, nyspeed)
    }

  }

  val SECTOR_PRECISION = 8

  class SectorMatrix(val boundaries: Boundaries, val sectorPrecision: Int) {
    val sectorSize = boundaries.size / sectorPrecision
    val matrix = new Array[ConcBuffer[Body]](sectorPrecision * sectorPrecision)
    for (i <- 0 until matrix.length) matrix(i) = new ConcBuffer

    // This method should use the body position, boundaries and sectorPrecision
    // to determine the sector into which the body should go into, and add the body
    // into the corresponding ConcBuffer object.
    // Importantly, if the Body lies outside of the Boundaries, it should be considered
    // to be located at the closest point within the Boundaries for the purpose
    // of finding which ConcBuffer should hold the body.
    def +=(b: Body): SectorMatrix = {
      val xAxis = Math.min((b.x - boundaries.minX) / sectorSize, sectorPrecision).toInt
      val yAxis = Math.min((b.y - boundaries.minY) / sectorSize, sectorPrecision).toInt
      apply(xAxis, yAxis) += b
      this
    }

    def apply(x: Int, y: Int) = matrix(y * sectorPrecision + x)

    // This method calls combine on the pair of ConcBuffers in this and that matrices
    // to produce the ConcBuffer for the resulting matrix. You can safely assume that
    // combine will only be called on matrices of same dimensions, boundaries and sector precision.
    def combine(that: SectorMatrix): SectorMatrix = {
      val combinedMatrix = new SectorMatrix(boundaries, sectorPrecision)
      for (i <- 0 until sectorPrecision) {
        for (j <- 0 until sectorPrecision) {
          val combinedConc = apply(i, j).combine(that.apply(i, j))
          combinedMatrix.matrix(j * sectorPrecision + i) = combinedConc
        }
      }
      combinedMatrix
    }

    def toQuad(parallelism: Int): Quad = {
      def BALANCING_FACTOR = 4
      def quad(x: Int, y: Int, span: Int, achievedParallelism: Int): Quad = {
        if (span == 1) {
          val sectorSize = boundaries.size / sectorPrecision
          val centerX = boundaries.minX + x * sectorSize + sectorSize / 2
          val centerY = boundaries.minY + y * sectorSize + sectorSize / 2
          var emptyQuad: Quad = Empty(centerX, centerY, sectorSize)
          val sectorBodies = this(x, y)
          sectorBodies.foldLeft(emptyQuad)(_ insert _)
        } else {
          val nspan = span / 2
          val nAchievedParallelism = achievedParallelism * 4
          val (nw, ne, sw, se) =
            if (parallelism > 1 && achievedParallelism < parallelism * BALANCING_FACTOR) parallel(
              quad(x, y, nspan, nAchievedParallelism),
              quad(x + nspan, y, nspan, nAchievedParallelism),
              quad(x, y + nspan, nspan, nAchievedParallelism),
              quad(x + nspan, y + nspan, nspan, nAchievedParallelism)
            ) else (
              quad(x, y, nspan, nAchievedParallelism),
              quad(x + nspan, y, nspan, nAchievedParallelism),
              quad(x, y + nspan, nspan, nAchievedParallelism),
              quad(x + nspan, y + nspan, nspan, nAchievedParallelism)
            )
          Fork(nw, ne, sw, se)
        }
      }

      quad(0, 0, sectorPrecision, 1)
    }

    override def toString = s"SectorMatrix(#bodies: ${matrix.map(_.size).sum})"
  }

  class TimeStatistics {
    private val timeMap = collection.mutable.Map[String, (Double, Int)]()

    def clear() = timeMap.clear()

    def timed[T](title: String)(body: =>T): T = {
      var res: T = null.asInstanceOf[T]
      val totalTime = /*measure*/ {
        val startTime = System.currentTimeMillis()
        res = body
        (System.currentTimeMillis() - startTime)
      }

      timeMap.get(title) match {
        case Some((total, num)) => timeMap(title) = (total + totalTime, num + 1)
        case None => timeMap(title) = (0.0, 0)
      }

      println(s"$title: ${totalTime} ms; avg: ${timeMap(title)._1 / timeMap(title)._2}")
      res
    }

    override def toString = {
      timeMap map {
        case (k, (total, num)) => k + ": " + (total / num * 100).toInt / 100.0 + " ms"
      } mkString("\n")
    }
  }
}
