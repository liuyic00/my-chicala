package chicala.convert.frontend

import scala.tools.nsc.Global

trait MStatementsLoader { self: Scala2Reader =>
  val global: Global
  import global._

  abstract class Loader[M <: MStatement] {
    def apply(cInfo: CircuitInfo, tr: Tree): Option[(CircuitInfo, Option[M])]
  }

  object MDefLoader extends Loader[MDef] {
    def apply(cInfo: CircuitInfo, tr: Tree): Option[(CircuitInfo, Option[MDef])] = {
      val (tree, tpt) = passThrough(tr)
      tree match {
        case _: ValDef => ValDefReader(cInfo, tr)
        case _: DefDef => DefDefReader(cInfo, tr)
      }
    }
  }

  def firstMatchIn[T <: MStatement](
      cInfo: CircuitInfo,
      tree: Tree,
      objs: List[(CircuitInfo, Tree) => Option[(CircuitInfo, Option[T])]]
  ): Option[(CircuitInfo, Option[T])] = {
    objs.foldLeft(None: Option[(CircuitInfo, Option[T])]) { case (past, obj) =>
      past match {
        case Some(_) => past
        case None    => obj(cInfo, tree)
      }
    }
  }

  /** Load multiple MStatements in a horizontal way, which means cInfo will only
    * update the readerInfo. Circut info such as new definitions will not be
    * updated in the cInfo.
    *
    * This can be used to the three parts of `if-then-else` or two sides of
    * `:=`.
    *
    * @param cInfo
    *   Inital CircuitInfo
    * @param funcs
    *   List of functions that load subtype of MStatement
    * @return
    *   new CircuitInfo and List of subtype of MStatement
    */
  def loadsWithUpdateReaderInfo[T <: MStatement](cInfo: CircuitInfo)(
      funcs: (CircuitInfo => Option[(CircuitInfo, Option[T])])*
  ): (CircuitInfo, List[T]) = {
    val t = funcs.foldLeft(
      (cInfo, List.empty[T])
    ) { case ((tCInfo, past), func) =>
      func(tCInfo) match {
        case Some((newCInfo, Some(mTerm))) => (cInfo.updatedWithReaderInfo(newCInfo), mTerm :: past)
        case _                             => throw new Exception("loadsWithUpdateReaderInfo failed")
      }
    }
    (t._1, t._2.reverse)
  }

  /** Load multiple MStatements in a vertical way, which means both circuit info
    * and reader info will be updated in the `cInfo`. Circut info such as new
    * definitions can be found in the new `cInfo`.
    *
    * This can be used on normal sequential statements.
    */
  // def loadVertical

}
