package basic.ast
//
import chisel3._
import chisel3.util._

class SubModule6() extends Module {
  val io = IO(new Bundle {
    val in  = Input(UInt(2.W))
    val out = Output(UInt(2.W))
  })
  val subModule = Module(new SubModule6p2())
  subModule.io.b := io.in
  subModule.io.a := io.in
  io.out         := subModule.io.c
}

class SubModule6p2() extends Module {
  val io = IO(new Bundle {
    val b = Input(UInt(2.W))
    val a = Input(UInt(2.W))
    val c = Output(UInt(2.W))
  })
  io.c := io.b + io.a
}
