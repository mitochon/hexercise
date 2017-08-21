package week6

object polynomials {


  class Poly(val terms: Map[Int, Double]) {
    def + (other: Poly) = new Poly(terms ++ (other.terms map addTerm))
    private def addTerm(term: (Int, Double)) = {
      val (degree, coeff) = term
      degree -> (coeff + terms(degree))
    }
    override def toString = {
      val termStrings = for ((pos, value) <- terms.toList.sorted.reverse) yield value+"x^"+pos
      termStrings mkString " + "
    }
  }
  def Poly(bindings: (Int, Double)*) = new Poly(bindings.toMap withDefaultValue 0)
                                                  //> Poly: (bindings: (Int, Double)*)week6.polynomials.Poly
  
  val p1 = Poly(1 -> 2.0, 3 -> 4.0, 5 -> 6.2)     //> p1  : week6.polynomials.Poly = 6.2x^5 + 4.0x^3 + 2.0x^1
  val p2 = Poly(0 -> 3.0, 3 -> 7.0)               //> p2  : week6.polynomials.Poly = 7.0x^3 + 3.0x^0
  p1 + p2                                         //> res0: week6.polynomials.Poly = 6.2x^5 + 11.0x^3 + 2.0x^1 + 3.0x^0
}