package basic.ast

import chisel3._

class AddFull(width: Int) extends Module {
  val io = IO(new Bundle {
    val in1 = Input(UInt(width.W))
    val in2 = Input(UInt(width.W))
    val out = Output(UInt((width + 1).W))
  })
  io.out := io.in1 +& io.in2
}
