package basic.ast

import chisel3._
import chisel3.util._

class AsTypeOfAst(width: Int = 2) extends Module {
  val io = IO(new Bundle {
    val in  = Input(UInt((width + 1).W))
    val out = Output(UInt(width.W))
  })
  io.out := io.in(width - 1, 0).asTypeOf(UInt(width.W))
}
