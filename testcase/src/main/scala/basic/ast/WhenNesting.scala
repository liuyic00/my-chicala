package basic.ast

import chisel3._
import chisel3.util._

class WhenNesting() extends Module {
  val io = IO(new Bundle {
    val in  = Input(Bool())
    val in1 = Input(Bool())
    val in2 = Input(Bool())
    val in3 = Input(Bool())
    val out = Output(UInt(2.W))
  })

  when(io.in) {
    io.out := 0.U
  }.elsewhen(io.in1) {
    when(io.in2) {
      io.out := 1.U
    }.elsewhen(io.in3) {
      io.out := 2.U
    }.otherwise {
      io.out := 3.U
    }
  }.otherwise {
    io.out := 4.U
  }
}
