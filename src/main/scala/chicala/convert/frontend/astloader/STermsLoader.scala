package chicala.convert.frontend

import scala.tools.nsc.Global

trait STermsLoader { self: Scala2Reader =>
  val global: Global
  import global._

  object STermLoader extends Loader[STerm] {
    def apply(cInfo: CircuitInfo, tr: Tree): LREitherT[ModifiedAndLoaded[STerm]] = {
      MTermLoader(cInfo, tr).asInstanceOf[LREitherT[ModifiedAndLoaded[STerm]]]
    }
  }

  object STupleLoader extends Loader[STuple] {
    def apply(cInfo: CircuitInfo, tr: Tree): LREither[STuple] = {
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
        case _ => Right(NotThis)
      }
    }
  }
  object SAssignLoader extends Loader[SAssign] {
    def apply(cInfo: CircuitInfo, tr: Tree): LREither[SAssign] = {
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
        case _ => Right(NotThis)
      }).flatMap {
        case ModifiedAndLoaded(newCInfo, left :: right :: Nil) =>
          Right(ModifiedAndLoaded(newCInfo, SAssign(left, right)))
        case NotThis => Right(NotThis)
        case _       => loadMutilpleMatchError(tree)
      }
    }
  }

  object SApplyLoader extends Loader[SApply] {
    def apply(cInfo: CircuitInfo, tr: Tree): LREither[SApply] = {
      val (tree, tpt) = passThrough(tr)
      tree match {
        case Apply(fun, args) =>
          MTermLoader
            .loadTerms(cInfo, fun :: args)
            .flatMap {
              case ModifiedAndLoaded(newCInfo, (sTerm: STerm) :: mArgs) =>
                Right(ModifiedAndLoaded(newCInfo, SApply(sTerm, mArgs, MTypeLoader.fromTpt(tpt).get)))
              case _ => loadMutilpleMatchError(fun)
            }
        case _ => Right(NotThis)
      }
    }
  }
}
