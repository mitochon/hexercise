package week8
import java.util.Random
import Generators._

object random {

  val rand = new Random                           //> rand  : java.util.Random = java.util.Random@48ee22f7
  rand.nextInt                                    //> res0: Int = -1830041461

  property("(xs take n) ++ (xs drop n) == xs") {
    choose(0, 2) { n =>
      ForAll { (xs: List[Int]) =>
        (xs take n) ++ (xs drop (n + 1)) == xs
      }
    }
  }                                               //> testing (xs take n) ++ (xs drop n) == xs
                                                  //| testing with 1
                                                  //| testing with List(283605566, 1419702890, -1715216886, -1787596589, -12931476
                                                  //| 0, 1029456325, -278215944, -762995277)
                                                  //| java.lang.AssertionError: assertion failed: test failed for List(283605566, 
                                                  //| 1419702890, -1715216886, -1787596589, -129314760, 1029456325, -278215944, -7
                                                  //| 62995277)
                                                  //| 	at scala.Predef$.assert(Predef.scala:179)
                                                  //| 	at week8.Generators$.ForAll(Generators.scala:29)
                                                  //| 	at week8.random$$anonfun$main$1$$anonfun$apply$mcV$sp$1$$anonfun$apply$m
                                                  //| cZ$sp$1.apply$mcZI$sp(week8.random.scala:12)
                                                  //| 	at week8.Generators$.choose(Generators.scala:22)
                                                  //| 	at week8.random$$anonfun$main$1$$anonfun$apply$mcV$sp$1.apply$mcZ$sp(wee
                                                  //| k8.random.scala:11)
                                                  //| 	at week8.Generators$$anonfun$property$1.apply$mcZI$sp(Generators.scala:3
                                                  //| 6)
                                                  //| 	at week8.Generators$$anonfun$property$1.apply(Generators.scala:35)
                                                  //| 	at week8.Generators$$anonfun$property$1.apply(Generators.scala:35)
                                                  //| 	at scala.collectio
                                                  //| Output exceeds cutoff limit.
}