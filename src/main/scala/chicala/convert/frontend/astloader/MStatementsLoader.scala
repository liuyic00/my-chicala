package chicala.convert.frontend

import scala.tools.nsc.Global
import org.w3c.dom.ls.LSResourceResolver

trait MStatementsLoader { self: Scala2Reader =>
  val global: Global
  import global._

  abstract class Loader[+M] {
    def apply(cInfo: CircuitInfo, tr: Tree): Either[LRAllLeft, LRSuccess[M]]
    def must(cInfo: CircuitInfo, tr: Tree): Either[LRError, LRSuccess[M]] = {
      apply(cInfo, tr).left.map {
        case NotThis =>
          unprocessedTree(tr, "must")
          Failed
        case x: LRError => x
      }
    }
  }

  abstract class LoadedLoader[+M] extends Loader[M] {
    override def apply(cInfo: CircuitInfo, tr: Tree): Either[LRAllLeft, Loaded[M]]
    override def must(cInfo: CircuitInfo, tr: Tree): Either[LRError, Loaded[M]] = {
      apply(cInfo, tr).left.map {
        case NotThis =>
          unprocessedTree(tr, "must")
          Failed
        case x: LRError => x
      }
    }
  }

  abstract class Reader[+M] {
    def apply(cInfo: CircuitInfo, tr: Tree): Either[LRError, LRSuccess[M]]
  }

  object MDefLoader extends Loader[MDef] {
    def apply(cInfo: CircuitInfo, tr: Tree): Either[LRAllLeft, LRSuccess[MDef]] = {
      val (tree, tpt) = passThrough(tr)
      tree match {
        case _: ValDef => ValDefReader(cInfo, tr)
        case _: DefDef => DefDefReader(cInfo, tr)
        case _         => Left(NotThis)
      }
    }
  }

  def firstMatchIn[T, CC[T]](
      cInfo: CircuitInfo,
      tree: Tree,
      objs: List[(CircuitInfo, Tree) => Either[LRAllLeft, CC[T]]]
  ): Either[LRAllLeft, CC[T]] = {
    LRSuccess.getFirst(objs.map(f => () => f(cInfo, tree)))
  }

  /** Load mutilple objects without modify cInfo.
    *
    * @return
    *   list with same size of `funcs`.
    */
  def loadMutilple[T](
      cInfo: CircuitInfo
  )(funcs: (CircuitInfo => Either[LRError, LRSuccess[T]])*): Either[LRError, Loaded[List[T]]] = {
    funcs
      .foldLeft(Right(List.empty[T]): Either[LRError, List[T]]) {
        case (Right(ls), f) =>
          f(cInfo).flatMap {
            case Loaded(x) => Right(x :: ls)
            case _ =>
              errorTree(EmptyTree, "loadMutilple")
              Left(Failed)
          }
        case (Left(x), f) => Left(x)
      }
      .map(x => Loaded(x.reverse))
  }

  /** Use this when `loadMutilple` returns a list witch failed match.
    */
  def loadMutilpleMatchError(tree: Tree) = {
    reporter.error(tree.pos, "loadMutilple match failed")
    Left(Failed)
  }

}
