package chicala.ast.util

import scala.tools.nsc.Global

import chicala.ast.ChicalaAst

trait Compares { self: ChicalaAst =>
  val global: Global
  import global._

  def sameKnownSignalType(tpeA: MType, tpeB: MType): Boolean = {
    val r = (tpeA, tpeB) match {
      case (a: SignalType, b: SignalType) => a.allSizeKnown && a.nomalize == b.nomalize
      case _                              => false
    }
    r
  }

}
