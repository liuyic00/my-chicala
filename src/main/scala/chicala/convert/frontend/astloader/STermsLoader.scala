package chicala.convert.frontend

import scala.tools.nsc.Global

trait STermsLoader { self: Scala2Reader =>
  val global: Global
  import global._

  object STermLoader extends LoadedLoader[STerm] {
    def apply(cInfo: CircuitInfo, tr: Tree): Either[LRAllLeft, Loaded[STerm]] = {
      MTermLoader(cInfo, tr).asInstanceOf[Either[LRAllLeft, Loaded[STerm]]]
    }
  }

  object STupleLoader extends LoadedLoader[STuple] {
    def apply(cInfo: CircuitInfo, tr: Tree): Either[LRAllLeft, Loaded[STuple]] = {
      val (tree, tpt) = passThrough(tr)
      tree match {
        case Apply(fun, args) if isScala2TupleApply(fun) =>
          MTermLoader
            .loadTerms(cInfo, args)
            .map(
              _.mapValue(
                STuple(
                  _,
                  MTypeLoader.fromTpt(tpt).get.asInstanceOf[StTuple]
                )
              )
            )
        case _ => Left(NotThis)
      }
    }
  }
  object SAssignLoader extends LoadedLoader[SAssign] {
    def apply(cInfo: CircuitInfo, tr: Tree): Either[LRAllLeft, Loaded[SAssign]] = {
      val (tree, _) = passThrough(tr)
      (tree match {
        case Assign(lhs, rhs) =>
          MTermLoader.loadTerms(cInfo, List(lhs, rhs))
        case Apply(Select(qualifier, name), args) if name.toString().endsWith("_$eq") =>
          val leftName = name.toString().dropRight(4)
          MTermLoader.loadTerms(
            cInfo,
            List(
              // this `TypeTree` only used for distinguish `SignalType` and other
              Typed(Select(qualifier, leftName), TypeTree(args.head.tpe)),
              args.head
            )
          )
        case _ => Left(NotThis)
      }).flatMap {
        case Loaded(left :: right :: Nil) =>
          Right(Loaded(SAssign(left, right)))
        case _ => loadMutilpleMatchError(tree)
      }
    }
  }

  object SApplyLoader extends LoadedLoader[SApply] {
    def apply(cInfo: CircuitInfo, tr: Tree): Either[LRAllLeft, Loaded[SApply]] = {
      val (tree, tpt) = passThrough(tr)
      tree match {
        case Apply(fun, args) =>
          MTermLoader
            .loadTerms(cInfo, fun :: args)
            .flatMap {
              case Loaded((sTerm: STerm) :: mArgs) =>
                Right(Loaded(SApply(sTerm, mArgs, MTypeLoader.fromTpt(tpt).get)))
              case _ => loadMutilpleMatchError(fun)
            }
        case _ => Left(NotThis)
      }
    }
  }
}
