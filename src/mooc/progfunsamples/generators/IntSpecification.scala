package week8

import org.scalacheck._
import Prop.forAll

object IntSpecification extends Properties("Int") {

  val ints = Gen.choose(-100, 100)

  property("sqrt") = forAll(ints) { d =>
    math.sqrt(d * d) == d
  }

}