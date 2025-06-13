package basic.ast
//
import chisel3._
import chisel3.util._

class SubModule5() extends Module {
  val io = IO(new Bundle {
    val in  = Input(UInt(2.W))
    val out = Output(UInt(2.W))
  })
  if (true) {
    val subModule = Module(new SubModule5p2(1))
    subModule.io.in := io.in
    io.out          := subModule.io.out
  } else {
    val subModule = Module(new SubModule5p2(2))
    subModule.io.in := io.in
    io.out          := subModule.io.out
  }
}

class SubModule5p2(add: Int) extends Module {
  val io = IO(new Bundle {
    val in  = Input(UInt(2.W))
    val out = Output(UInt(2.W))
  })
  io.out := io.in + add.U(2.W)
}
