package sv2chisel

import chisel3._
import chisel3.util._
import sv2chisel.helpers.vecconvert._

class VecConvert2() extends Module {

  val b = IO(Input(Vec(37, Bool())))
  val c = IO(Output(Vec(37, Bool())))
  val d = IO(Output(Vec(21, Bool())))

  c(36, 0) := b(36, 0).asTypeOf(c)
  d        := b(20, 0).asTypeOf(d)
  c(20, 0) := b(20, 0).asTypeOf(d)
}
