package sv2chisel

import chisel3._
import chisel3.util._
import sv2chisel.helpers.vecconvert._

class SomeValue(overrideK: Option[Int] = None) extends Module {
  val k = overrideK.getOrElse(2)
  val io = IO(new Bundle {
    val in  = Input(Vec(k, Bool()))
    val out = Output(Vec(k, Bool()))
  })
  io.out := io.in
}
