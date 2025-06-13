package basic.sort

import chisel3._

/** Split when body
  */
class Sort3(width: Int) extends Module {
  val io = IO(new Bundle { // 1
    val valid = Input(Bool())
    val in    = Input(UInt(width.W))
    val out   = Output(UInt(width.W))
  })

  val a = Wire(UInt(width.W)) // 2
  val b = Wire(UInt(width.W)) // 3
  val c = Wire(UInt(width.W)) // 4

  b := a

  when(io.valid) {
    a := io.in
    c := io.in
  }

}
