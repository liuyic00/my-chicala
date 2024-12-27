package example.xiangshan.multifile

import chisel3._
import chisel3.util._

class C53 extends Module {
  val io = IO(new Bundle() {
    val in  = Input(Vec(5, UInt(1.W)))
    val out = Output(Vec(3, UInt(1.W)))
  })
  val FAs0, FAs1 = Module(new C32)
  FAs0.io.in := io.in.take(3)
  val tmp1 = VecInit(FAs0.io.out(0), io.in(3), io.in(4))
  FAs1.io.in := tmp1
  val tmp2 = VecInit(FAs1.io.out(0), FAs0.io.out(1), FAs1.io.out(1))
  io.out := tmp2
}
