package chicala.ast.util

import scala.tools.nsc.Global

import chicala.ast.ChicalaAst

trait Computes { self: ChicalaAst =>
  val global: Global
  import global._

  def simplify(exp: STerm): STerm = {
    val sexp = simplifyInside(exp)
    sexp match {
      // a op b = a_op_b
      case SApply(SSelect(SLiteral(a: Int, StInt), TermName(op), StFunc), List(SLiteral(b: Int, StInt)), StInt) =>
        op match {
          case "$plus"  => SLiteral(a + b, StInt)
          case "$minus" => SLiteral(a - b, StInt)
          case "$div"   => SLiteral(a / b, StInt)
          case "$times" => SLiteral(a * b, StInt)
          case _        => sexp
        }
      // x - 0 = x
      // x + 0 = x
      case SApply(SSelect(x: STerm, TermName("$minus") | TermName("$plus"), StFunc), List(SLiteral(0, StInt)), StInt) =>
        x
      // a + x = x + a
      // a * x = x * a
      case SApply(
            SSelect(a: SLiteral, op @ (TermName("$times") | TermName("$plus")), StFunc),
            List(x: STerm),
            StInt
          ) =>
        simplify(SApply(SSelect(x, op, StFunc), List(a), StInt))
      // x op1 a op2 b
      case SApply(
            SSelect(
              SApply(SSelect(x: STerm, TermName(op1), StFunc), List(SLiteral(a: Int, StInt)), StInt),
              TermName(op2),
              StFunc
            ),
            List(SLiteral(b: Int, StInt)),
            StInt
          ) =>
        (op1, op2) match {
          case ("$plus", "$plus")   => simplify(plus(x, SLiteral(a + b, StInt)))
          case ("$minus", "$minus") => simplify(minus(x, SLiteral(a + b, StInt)))
          case ("$plus", "$minus") =>
            val v = a - b
            if (v >= 0) simplify(plus(x, SLiteral(v, StInt)))
            else simplify(minus(x, SLiteral(-v, StInt)))
          case ("$minus", "$plus") =>
            val v = b - a
            if (v >= 0) simplify(plus(x, SLiteral(v, StInt)))
            else simplify(minus(x, SLiteral(-v, StInt)))
          case _ => sexp
        }

      // (x1 + y) - (x2 + z) = y - z
      case SApply(
            SSelect(
              SApply(SSelect(x1: STerm, TermName("$plus"), StFunc), List(y: STerm), StInt),
              TermName("$minus"),
              StFunc
            ),
            List(
              SApply(SSelect(x2: STerm, TermName("$plus"), StFunc), List(z: STerm), StInt)
            ),
            StInt
          ) if x1 == x2 =>
        simplify(minus(y, z))
      // (x1 + y + z) - (x2) = y + z
      case SApply(
            SSelect(
              SApply(
                SSelect(
                  SApply(SSelect(x1: STerm, TermName("$plus"), StFunc), List(y: STerm), StInt),
                  TermName("$plus"),
                  StFunc
                ),
                List(z: STerm),
                StInt
              ),
              TermName("$minus"),
              StFunc
            ),
            List(x2: STerm),
            StInt
          ) if x1 == x2 =>
        simplify(plus(y, z))
      // x + y - z
      case SApply(
            SSelect(
              SApply(SSelect(x: STerm, TermName("$plus"), StFunc), List(y: STerm), StInt),
              TermName("$minus"),
              StFunc
            ),
            List(z: STerm),
            StInt
          ) =>
        if (x == z) y
        else if (y == z) x
        else sexp
      // x - y + z
      case SApply(
            SSelect(
              SApply(SSelect(x: STerm, TermName("$minus"), StFunc), List(y: STerm), StInt),
              TermName("$plus"),
              StFunc
            ),
            List(z: STerm),
            StInt
          ) =>
        if (y == z) x
        else sexp

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
