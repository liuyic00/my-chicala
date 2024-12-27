package example.xiangshan.multifile

import chisel3._
import chisel3.util._

class C22 extends Module {
  val io = IO(new Bundle() {
    val in  = Input(Vec(2, UInt(1.W)))
    val out = Output(Vec(2, UInt(1.W)))
  })
  val temp = Wire(Vec(1, UInt(2.W)))
  for (i <- 0 until temp.length) {
    val (a, b) = (io.in(0)(i), io.in(1)(i))
    val sum    = a ^ b
    val cout   = a & b
    temp(i) := Cat(cout, sum)
  }
  for (i <- 0 until io.out.length) {
    io.out(i) := Cat(temp.reverse map (_(i)))
  }
}
