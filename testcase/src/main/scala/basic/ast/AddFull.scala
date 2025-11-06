package basic.ast

import chisel3._

class AddFull(width: Int) extends Module {
  val io = IO(new Bundle {
    val in1  = Input(UInt(width.W))
    val in2  = Input(UInt(width.W))
    val out1 = Output(UInt((width + 1).W))
    val out2 = Output(UInt((width).W))
  })
  io.out1 := io.in1 +& io.in2
  io.out2 := io.in1 +& io.in2
}
