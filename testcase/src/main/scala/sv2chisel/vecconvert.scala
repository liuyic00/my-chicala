package sv2chisel

import chisel3._
import chisel3.util._
import sv2chisel.helpers.vecconvert._

class VecConvert(width1: Int, width2: Int) extends Module {

  val a = IO(Input(Bool()))
  val b = IO(Input(Vec(width1 + 1, Bool())))
  val c = IO(Output(Vec(width1 + 1, Bool())))

  c(width1, 0) := (
    VecInit.tabulate(width1 + 1)(_ => a).asUInt
      & Fill(width1 + 1, true.B)
  ).asTypeOf(Vec(width1 + 1, Bool()))

  c(width1, 0) := b(width1, 0).asUInt.asTypeOf(Vec(width1 + 1, Bool()))

}
