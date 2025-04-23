package chicala.util

import scala.tools.nsc.Global

trait Printer extends Format {
  val global: Global
  import global._

  object logger {
    var indent                  = 0
    var store: Map[String, Any] = Map.empty
    var enable                  = true

    def log(msg: String) = if (enable) global.log("| " * indent + msg)

    /** Print messages for debugging and should be removed after debugging. */
    def debug(msg: String) = log(msg)
    def in(msg: String) = {
      log(s"in ${msg}")
      indent += 1
    }
    def out(msg: String) = {
      indent -= 1
      log(s"out ${msg}")
    }
  }

  def unprocessedTree(tree: Tree, from: String) = {
    reporter.error(
      tree.pos,
      s"""not processed in ${from}:
        |tree.tpe:
        |  ${tree.tpe.erasure}
        |tree:
        |  ${tree}
        |tree AST:
        |  ${showFormattedRaw(tree).replace("\n", "\n  ")}
        |source code:""".stripMargin
    )
  }

  def errorTree(tree: Tree, msg: String) = {
    val slicedTraces = stackTraces.drop(3).take(10).reduce(_ + "\n  " + _)
    reporter.error(
      tree.pos,
      s"""${msg}:
        |tree.tpe:
        |  ${tree.tpe.erasure}
        |tree AST:
        |  ${showFormattedRaw(tree).replace("\n", "\n  ")}
        |stackTrace:
        |  ${slicedTraces}
        |  ...
        |source code:""".stripMargin
    )
  }

  def stackTraces = {
    Thread
      .currentThread()
      .getStackTrace
      .map(_.toString())
      .toList
      .drop(4)
  }

  def echoTreePos(tree: Tree) = {
    reporter.echo(tree.pos, "here")
  }

  def assertWarning(cond: Boolean, pos: Position, msg: String) = if (!cond) {
    reportWarning(pos, msg, 3)
  }
  def assertError(cond: Boolean, pos: Position, msg: String) = if (!cond) {
    reportError(pos, msg, 3)
  }

  def reportWarning(pos: Position, msg: String, tracesExtDrop: Int = 0) = {
    reporter.warning(
      pos,
      s"""${msg}
         |  ${stackTraces.drop(3 + tracesExtDrop).take(10).mkString("\n  ")}
         |  ...""".stripMargin
    )
  }
  def reportError(pos: Position, msg: String, tracesExtDrop: Int = 0) = {
    reporter.error(
      pos,
      s"""${msg}
         |  ${stackTraces.drop(3 + tracesExtDrop).take(10).mkString("\n  ")}
         |  ...""".stripMargin
    )
  }

  object TODO {
    def apply(from: String, msg: Any, tpe: Any): String = {
      val m = s"TODO($from, $msg, $tpe)"
      reportWarning(NoPosition, m, 1)
      m
    }
    def apply(from: String, msg: Any): String = {
      val m = s"TODO($from, $msg)"
      reportWarning(NoPosition, m, 1)
      m
    }
  }
  object Unsupport {
    def apply(from: String, msg: Any): String = {
      val m = s"Unsupport($from, $msg)"
      reportError(NoPosition, m, 1)
      m
    }
  }

  implicit class NormalTermName(tn: TermName) {
    def normal: String = {
      val s = tn.toString
      if (s == "package") s"`${s}`"
      else s
    }
  }
}
