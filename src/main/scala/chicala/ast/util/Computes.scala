package chicala.ast.util

import scala.tools.nsc.Global

import chicala.ast.ChicalaAst

trait Computes { self: ChicalaAst =>
  val global: Global
  import global._

  def simplify(x: STerm): STerm = {
    val sx = simplifyInside(x)
    sx match {
      // a + b - c
      case SApply(
            SSelect(
              SApply(SSelect(a: STerm, TermName("$plus"), StFunc), List(b: STerm), StInt),
              TermName("$minus"),
              StFunc
            ),
            List(c: STerm),
            StInt
          ) if b == c =>
        a
      // a - b + c
      case x @ SApply(
            SSelect(
              SApply(SSelect(a: STerm, TermName("$minus"), StFunc), List(b: STerm), StInt),
              TermName("$plus"),
              StFunc
            ),
            List(c: STerm),
            StInt
          ) if b == c =>
        a
      // a - 0
      case SApply(SSelect(a: STerm, TermName("$minus"), StFunc), List(SLiteral(0, StInt)), StInt) =>
        a
      // a + 0
      case SApply(SSelect(a: STerm, TermName("$plus"), StFunc), List(SLiteral(0, StInt)), StInt) =>
        a
      case _ => sx
    }
  }
  def simplifyInside(x: STerm): STerm = {
    x match {
      case SApply(SSelect(a: STerm, op, StFunc), List(b: STerm), StInt) =>
        SApply(SSelect(simplify(a), op, StFunc), List(simplify(b)), StInt)
      case _ => x
    }

  }

  def plus(x: STerm, y: STerm): STerm = {
    SApply(
      SSelect(x, TermName("$plus"), StFunc),
      List(y),
      StInt
    )
  }
  def minus(x: STerm, y: STerm): STerm = {
    SApply(
      SSelect(x, TermName("$minus"), StFunc),
      List(y),
      StInt
    )
  }

  def leftRightSize(l: STerm, r: STerm): STerm = {
    simplify(minus(plus(l, SLiteral(1, StInt)), r))
  }
}
