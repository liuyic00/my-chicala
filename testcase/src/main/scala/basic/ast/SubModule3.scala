package basic.ast
//
import chisel3._
import chisel3.util._

class SubModule3() extends Module {
  val io = IO(new Bundle {
    val in  = Input(UInt(2.W))
    val out = Output(UInt(2.W))
  })
  // the middle one `b` use default value
  // this will make scala expand "new ..." to a block
  val subModule = Module(new SubModule3p2(a = 1, c = 1))
  subModule.io.in := io.in
  io.out          := subModule.io.out
}

class SubModule3p2(
    val a: Int = 1,
    val b: Int = 1,
    val c: Int = 1
) extends Module {
  val io = IO(new Bundle {
    val in  = Input(UInt(2.W))
    val out = Output(UInt(2.W))
  })
  io.out := io.in
}
