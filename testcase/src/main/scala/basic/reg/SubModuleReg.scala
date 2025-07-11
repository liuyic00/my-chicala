package basic.reg

import chisel3._

class SubModuleReg1(width1: Int) extends Module {
  val in1  = IO(Input(UInt(width1.W)))
  val out1 = IO(Output(UInt(width1.W)))
  val reg1 = RegInit(1.U(width1.W))

  val sub = Module(new SubModuleReg1p2(width1))
  sub.in2 := in1
  reg1    := sub.out2
  out1    := reg1
}

class SubModuleReg1p2(width2: Int) extends Module {
  val in2  = IO(Input(UInt(width2.W)))
  val out2 = IO(Output(UInt(width2.W)))
  val reg2 = RegInit(2.U(width2.W))

  reg2 := in2
  out2 := reg2
}
