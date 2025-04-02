package basic.ast

import chisel3._
import chisel3.util._

class SubModule1(width: Int, size: Int) extends Module {
  val io = IO(new Bundle {
    val in  = Input(Vec(size, UInt(width.W)))
    val out = Output(Vec(size, UInt(width.W)))
  })

  for (i <- 0 until size) {
    val subModule = Module(new SubModule1p2(width))
    subModule.io.in := io.in(i)
    io.out(i)       := subModule.io.out
  }
}

class SubModule1p2(width: Int) extends Module {
  val io = IO(new Bundle {
    val in  = Input(UInt(width.W))
    val out = Output(UInt(width.W))
  })

  io.out := io.in
}
