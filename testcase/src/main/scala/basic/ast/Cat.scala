package basic.ast

import chisel3._
import chisel3.util._

class CatAst() extends Module {
  val io = IO(new Bundle {
    val in1 = Input(UInt(3.W))
    val in2 = Input(UInt(3.W))
    val out = Output(Vec(2, UInt(4.W)))
  })
  io.out(0) := Cat(io.in1(0), io.in2(2, 0))
}
