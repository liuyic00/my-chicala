package chicala.ast.util

import scala.tools.nsc.Global

import chicala.ast.ChicalaAst

trait Computes { self: ChicalaAst =>
  val global: Global
  import global._

  def simplify(exp: STerm): STerm = {
    val sexp = simplifyInside(exp)
    sexp match {
      // x + y1 - y2
      case SApply(
            SSelect(
              SApply(SSelect(x: STerm, TermName("$plus"), StFunc), List(y1: STerm), StInt),
              TermName("$minus"),
              StFunc
            ),
            List(y2: STerm),
            StInt
          ) if y1 == y2 =>
        x
      // x - y1 + y2
      case SApply(
            SSelect(
              SApply(SSelect(x: STerm, TermName("$minus"), StFunc), List(y1: STerm), StInt),
              TermName("$plus"),
              StFunc
            ),
            List(y2: STerm),
            StInt
          ) if y1 == y2 =>
        x
      // x - 0 = x
      // x + 0 = x
      case SApply(SSelect(x: STerm, TermName("$minus") | TermName("$plus"), StFunc), List(SLiteral(0, StInt)), StInt) =>
        x

      // Compute Litteral
      // a + b = (a+b)
      // a - b = (a-b)
      case SApply(SSelect(SLiteral(a: Int, StInt), TermName(op), StFunc), List(SLiteral(b: Int, StInt)), StInt) =>
        op match {
          case "$plus"  => SLiteral(a + b, StInt)
          case "$minus" => SLiteral(a - b, StInt)
          case "$div"   => SLiteral(a / b, StInt)
          case "$times" => SLiteral(a * b, StInt)
          case _        => sexp
        }
      case _ => sexp
    }
  }
  def simplifyInside(exp: STerm): STerm = {
    exp match {
      case SApply(SSelect(x: STerm, op, StFunc), List(y: STerm), StInt) =>
        SApply(SSelect(simplify(x), op, StFunc), List(simplify(y)), StInt)
      case _ => exp
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

  def isEqual(x: STerm, y: STerm): Boolean = {
    x == y || simplify(x) == simplify(y)
  }

  def leftRightSize(l: STerm, r: STerm): STerm = {
    simplify(minus(plus(l, SLiteral(1, StInt)), r))
  }
}
