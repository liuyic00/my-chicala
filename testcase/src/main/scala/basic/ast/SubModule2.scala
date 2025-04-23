package basic.ast
//
import chisel3._
import chisel3.util._

class SubModule2(width: Int = 2) extends Module {
  val io = IO(new Bundle {
    val in  = Input(UInt(width.W))
    val out = Output(UInt(width.W))
  })
  val subModule = Module(new SubModule2p2)
  subModule.io.in := io.in
  io.out          := subModule.io.out
}

class SubModule2p2(width: Int = 2) extends Module {
  val io = IO(new Bundle {
    val in  = Input(UInt(width.W))
    val out = Output(UInt(width.W))
  })
  io.out := io.in
}
