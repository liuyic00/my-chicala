package basic.ast

import chisel3._
import chisel3.util._

class MultiIO(width1: Int, width2: Int) extends Module {
  val a = IO(Input(Vec(width1, Bool())))
  val b = IO(Input(Vec(width2, Bool())))
  val c = IO(Output(Vec(width2, Bool())))

  c := b
}
