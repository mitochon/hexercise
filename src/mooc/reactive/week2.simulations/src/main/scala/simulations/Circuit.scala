package simulations

import common._

class Wire {
  private var sigVal = false
  private var actions: List[Simulator#Action] = List()

  def getSignal: Boolean = sigVal
  
  def setSignal(s: Boolean) {
    if (s != sigVal) {
      sigVal = s
      actions.foreach(action => action())
    }
  }

  def addAction(a: Simulator#Action) {
    actions = a :: actions
    a()
  }
}

abstract class CircuitSimulator extends Simulator {

  val InverterDelay: Int
  val AndGateDelay: Int
  val OrGateDelay: Int

  def probe(name: String, wire: Wire) {
    wire addAction {
      () => afterDelay(0) {
        println(
          "  " + currentTime + ": " + name + " -> " +  wire.getSignal)
      }
    }
  }

  def inverter(input: Wire, output: Wire) {
    def invertAction() {
      val inputSig = input.getSignal
      afterDelay(InverterDelay) { output.setSignal(!inputSig) }
    }
    input addAction invertAction
  }

  def andGate(a1: Wire, a2: Wire, output: Wire) {
    def andAction() {
      val a1Sig = a1.getSignal
      val a2Sig = a2.getSignal
      afterDelay(AndGateDelay) { output.setSignal(a1Sig & a2Sig) }
    }
    a1 addAction andAction
    a2 addAction andAction
  }

  //
  // to complete with orGates and demux...
  //

  def orGate(a1: Wire, a2: Wire, output: Wire) {
    def orAction() {
      val a1Sig = a1.getSignal
      val a2Sig = a2.getSignal
      afterDelay(OrGateDelay) { output.setSignal(a1Sig | a2Sig) }
    }
    a1 addAction orAction
    a2 addAction orAction
  }
  
  def orGate2(a1: Wire, a2: Wire, output: Wire) {
    // (a || b) = !(!a && !b)
    val a1Inv, a2Inv, outInv = new Wire
    inverter(a1, a1Inv)
    inverter(a2, a2Inv)
    andGate(a1Inv, a2Inv, outInv)
    inverter(outInv, output)
  }

  def demux(in: Wire, c: List[Wire], out: List[Wire]) {
    def base0(in: Wire, o: Wire) = {
      val inv = new Wire
      inverter(in, inv)
      inverter(inv, o)
    }
    def base1(in: Wire, c1: Wire, o1: Wire, o2: Wire) = {
      val cInv = new Wire
      inverter(c1, cInv)
      andGate(in, c1, o1)
      andGate(in, cInv, o2)
    }
    def demux1(in: Wire, c: List[Wire], out: List[Wire]): Unit = (c, out) match {
      case (Nil, o1 :: Nil) => 
        base0(in, o1)
      case (c1 :: Nil, o1 :: o2 :: Nil) =>
        base1(in, c1, o1, o2)
      case (c1 :: cs, out) =>
        val tmp = List.fill(out.length / 2) (new Wire)
        demux1(in, cs, tmp)
        val (lf, rt) = out splitAt(out.length / 2)
        //demux0(tmp, c1, out)
        demux2(tmp, c1, lf, rt)
      case _ =>
        throw new IllegalArgumentException("bad number of input and output wires")
    }
    def demux2(in: List[Wire], c: Wire, out1: List[Wire], out2: List[Wire]): Unit = (in, out1, out2) match {
      case (c1 :: cs, o1 :: os1, o2 :: os2) =>
        base1(c1, c, o1, o2)
        demux2(cs, c, os1, os2)
      case (Nil, Nil, Nil) =>
      case _ =>
        throw new IllegalArgumentException("bad number of input and output wires")
    }
    demux1(in, c, out)
  }
}

object Circuit extends CircuitSimulator {
  val InverterDelay = 1
  val AndGateDelay = 3
  val OrGateDelay = 5

  def andGateExample {
    val in1, in2, out = new Wire
    andGate(in1, in2, out)
    probe("in1", in1)
    probe("in2", in2)
    probe("out", out)
    in1.setSignal(false)
    in2.setSignal(false)
    run

    in1.setSignal(true)
    run

    in2.setSignal(true)
    run
  }

  //
  // to complete with orGateExample and demuxExample...
  //
  // Courtesy of Bill Barrington via https://class.coursera.org/reactive-001/forum/thread?thread_id=473
  
  def demux0 {
    val in, out = new Wire
    demux(in, Nil, List(out))
    in.setSignal(false)
    run

    assert(out.getSignal == false, "demux0 1")

    in.setSignal(true)
    run

    assert(out.getSignal == true, "demux0 2")
  }

  def demux1 {
    val in, c, o0, o1 = new Wire
    demux(in, List(c), List(o1, o0))
    in.setSignal(false)
    c.setSignal(false)
    run

    assert(o0.getSignal == false, "demux1 1")
    assert(o1.getSignal == false, "demux1 1")

    in.setSignal(true)
    run

    assert(o0.getSignal == true, "demux1 2")
    assert(o1.getSignal == false, "demux1 2")

    c.setSignal(true)
    run

    assert(o0.getSignal == false, "demux1 3")
    assert(o1.getSignal == true, "demux1 3")

    in.setSignal(false)
    run

    assert(o0.getSignal == false, "demux1 4")
    assert(o1.getSignal == false, "demux1 4")
  }

  def demux2 {
    val in, c0, c1, o0, o1, o2, o3 = new Wire
    demux(in, List(c1, c0), List(o3, o2, o1, o0))
    in.setSignal(false)
    c0.setSignal(false)
    c1.setSignal(false)
    run

    assert(o0.getSignal == false, "demux2 1")
    assert(o1.getSignal == false, "demux2 1")
    assert(o2.getSignal == false, "demux2 1")
    assert(o3.getSignal == false, "demux2 1")

    in.setSignal(true)
    run

    assert(o0.getSignal == true, "demux2 2")
    assert(o1.getSignal == false, "demux2 2")
    assert(o2.getSignal == false, "demux2 2")
    assert(o3.getSignal == false, "demux2 2")

    c1.setSignal(true)
    run

    assert(o0.getSignal == false, "demux2 3")
    assert(o1.getSignal == false, "demux2 3")
    assert(o2.getSignal == true, "demux2 3")
    assert(o3.getSignal == false, "demux2 3")

    in.setSignal(false)
    run

    assert(o0.getSignal == false, "demux2 4")
    assert(o1.getSignal == false, "demux2 4")
    assert(o2.getSignal == false, "demux2 4")
    assert(o3.getSignal == false, "demux2 4")
  }
  
  // small test case for debugging
  def demux2debug {
    val in, c0, c1, o0, o1, o2, o3 = new Wire
    demux(in, List(c1,c0), List(o3,o2,o1,o0))
    in.setSignal(true)
    c1.setSignal(false)
    c0.setSignal(true)
    run
        
    probe("in", in)
    probe("c0", c0)
    probe("c1", c1) 
    
    probe("o0", o0)
    probe("o1", o1)
    probe("o2", o2)
    probe("o3", o3)
    
    run
  }  
}

object CircuitMain extends App {
  // You can write tests either here, or better in the test class CircuitSuite.
  Circuit.andGateExample
  Circuit.demux0
  Circuit.demux1
  Circuit.demux2
}
