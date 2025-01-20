package chicala.convert.frontend

import scala.tools.nsc.Global
import org.w3c.dom.ls.LSResourceResolver

trait MStatementsLoader { self: Scala2Reader =>
  val global: Global
  import global._

  abstract class Loader[+M] {
    def apply(cInfo: CircuitInfo, tr: Tree): LREither[M]
  }

  object MDefLoader extends Loader[MDef] {
    def apply(cInfo: CircuitInfo, tr: Tree): LREitherSuccess[MDef] = {
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
      objs: List[(CircuitInfo, Tree) => LREither[T]]
  ): LREither[T] = {
    LRSuccess.getFirst(objs.map(f => () => f(cInfo, tree)))
  }

  def loadMutilple[T](cInfo: CircuitInfo)(funcs: (CircuitInfo => LREither[T])*): LREitherLoaded[List[T]] = {
    funcs
      .foldLeft(Right(List.empty[T]): Either[LRError, List[T]]) {
        case (Right(ls), f) =>
          f(cInfo) match {
            case Right(Loaded(_, x)) => Right(x :: ls)
            case Right(_) =>
              errorTree(EmptyTree, "loadMutilple")
              Left(Failed)
            case Left(x) => Left(x)
          }
        case (Left(x), f) => Left(x)
      }
      .map(x => Loaded(cInfo, x.reverse))
  }

  def loadMutilpleMatchError(tree: Tree) = {
    reporter.error(tree.pos, "loadMutilple match failed")
    Left(Failed)
  }

  /** Load multiple MStatements in a vertical way, which means both circuit info
    * and reader info will be updated in the `cInfo`. Circut info such as new
    * definitions can be found in the new `cInfo`.
    *
    * This can be used on normal sequential statements.
    */
  // def loadVertical

}
