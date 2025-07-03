package pass

import chisel3._
import chisel3.util._
import sv2chisel.helpers.vecconvert._

class LitZero() extends Module {
  val a = IO(Input(UInt(8.W)))
  val b = IO(Output(UInt(8.W)))
  b := 0.U.asTypeOf(a)
}
