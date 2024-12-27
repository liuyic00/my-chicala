package example.xiangshan.multifile

import chisel3._
import chisel3.util._

class C32 extends Module {
  val io = IO(new Bundle() {
    val in  = Input(Vec(3, UInt(1.W)))
    val out = Output(Vec(2, UInt(1.W)))
  })
  val temp = Wire(Vec(1, UInt(2.W)))
  for (i <- 0 until temp.length) {
    val (a, b, cin) = (io.in(0)(i), io.in(1)(i), io.in(2)(i))
    val a_xor_b     = a ^ b
    val a_and_b     = a & b
    val sum         = a_xor_b ^ cin
    val cout        = a_and_b | (a_xor_b & cin)
    temp(i) := Cat(cout, sum)
  }
  for (i <- 0 until io.out.length) {
    io.out(i) := Cat(temp.reverse map (_(i)))
  }
}
