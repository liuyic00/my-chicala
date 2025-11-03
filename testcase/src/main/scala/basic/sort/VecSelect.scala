package basic.sort

import chisel3._

class VecSelect(width: Int) extends Module {
  val in  = IO(Input(Vec(2, UInt(width.W))))
  val out = IO(Output(Vec(2, UInt(width.W))))

  val t = Wire(Vec(2, UInt(width.W)))
  out(0) := t(0)
  out(1) := t(1)
  t(0)   := in(0)
  t(1)   := in(1)
}
