package sv2chisel

import chisel3._
import chisel3.util._
import sv2chisel.helpers.vecconvert._

class Sub(subWidth: Int) extends Module {
  val b = IO(Input(Vec(subWidth + 1, Bool())))
  val c = IO(Output(Vec(subWidth + 1, Bool())))
  c := b
}

class Top(width1: Int) extends Module {
  val b = IO(Input(Vec(width1 + 1, Bool())))
  val c = IO(Output(Vec(width1 + 1, Bool())))

  val subModule = Module(new Sub(width1))
  subModule.b  := b(width1, 0).asTypeOf(subModule.b)
  c(width1, 0) := subModule.c(width1, 0)

}
