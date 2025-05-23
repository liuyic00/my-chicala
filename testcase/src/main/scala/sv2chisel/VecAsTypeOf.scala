package sv2chisel

import chisel3._
import chisel3.util._
import sv2chisel.helpers.vecconvert._

class VecAsTypeOf(k: Int = 2) extends Module {
  val io = IO(new Bundle {
    val in  = Input(Vec(k + 1, Bool()))
    val out = Output(Vec(k, Bool()))
  })
  io.out := io.in(k - 1, 0).asTypeOf(UInt(k.W))
}
