package basic.ast
//
import chisel3._
import chisel3.util._

class SubModule4() extends Module {
  val io = IO(new Bundle {
    val in  = Input(UInt(2.W))
    val out = Output(UInt(2.W))
  })
  val subModule = Module(new SubModule4p2(widthOverride = Some(2)))
  subModule.io.in := io.in
  io.out          := subModule.io.out
}

class SubModule4p2(
    val widthOverride: Option[Int] = None
) extends Module {
  val width = widthOverride.getOrElse(1)
  val io = IO(new Bundle {
    val in  = Input(UInt(width.W))
    val out = Output(UInt(width.W))
  })
  io.out := io.in
}
