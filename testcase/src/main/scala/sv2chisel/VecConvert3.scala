package sv2chisel

import chisel3._
import chisel3.util._
import sv2chisel.helpers.vecconvert._

class VecConvert3Sub(subWidth: Int = 10) extends Module {
  val a = IO(Input(Vec(subWidth, Bool())))
  val b = IO(Output(Vec(subWidth + 1, Bool())))
  b := VecInit.tabulate(subWidth + 1)(i => true.B)
}

class VecConvert3() extends Module {
  val a = IO(Input(Vec(37, Bool())))
  val b = IO(Output(Vec(37 + 1, Bool())))

  for (i <- 0 until 2) {
    val sub = Module(new VecConvert3Sub(subWidth = 37))
    sub.a    := a(36, 0).asTypeOf(sub.a)
    b(37, 0) := sub.b(37, 0).asTypeOf(b)
  }
}
