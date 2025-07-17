package basic.reg

import chisel3._

class RegVec(width: Int, depth: Int) extends Module {
  val in  = IO(Input(UInt(width.W)))
  val out = IO(Output(UInt(width.W)))
  val rst = IO(Input(Bool()))

  val regs = RegInit(VecInit(Seq.fill(depth)(0.U(width.W))))

  for (i <- 0 until depth) {
    if (i == 0) {
      regs(i) := in
    } else {
      when(rst) {
        regs(i) := 0.U
      }.otherwise {
        regs(i) := regs(i - 1)
      }
    }
  }
  out := regs(depth - 1)
}
