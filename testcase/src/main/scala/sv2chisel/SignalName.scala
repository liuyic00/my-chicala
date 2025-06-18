package sv2chisel

import chisel3._
import chisel3.util._
import sv2chisel.helpers.vecconvert._

class SignalNameP1() extends Module {
  val a = IO(Input(UInt(8.W)))
  val b = IO(Output(UInt(8.W)))

  val sub = Module(new SignalNameP2())
  sub.IN := a
  b      := sub.OUT
}
class SignalNameP2() extends Module {
  val IN  = IO(Input(UInt(8.W)))
  val OUT = IO(Output(UInt(8.W)))
  OUT := IN
}
