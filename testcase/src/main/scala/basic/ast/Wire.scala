package basic.ast

import chisel3._
import chisel3.util._

class WireAst(width: Int = 2) extends Module {
  val io = IO(new Bundle {
    val in  = Input(UInt(width.W))
    val out = Output(UInt(width.W))
  })
  val wire = WireDefault(UInt(width.W), 0.U(width.W))
  wire   := io.in
  io.out := wire
}
