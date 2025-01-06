package chicala.convert.frontend

import scala.tools.nsc.Global

trait STermsLoader { self: Scala2Reader =>
  val global: Global
  import global._

  object STermLoader extends Loader[STerm] {
    def apply(cInfo: CircuitInfo, tr: Tree): Option[(CircuitInfo, Option[STerm])] = {
      MTermLoader(cInfo, tr).asInstanceOf[Option[(CircuitInfo, Option[STerm])]]
    }
  }

  object STupleLoader extends Loader[STuple] {
    def apply(cInfo: CircuitInfo, tr: Tree): Option[(CircuitInfo, Option[STuple])] = {
      val (tree, tpt) = passThrough(tr)
      tree match {
        case Apply(fun, args) if isScala2TupleApply(fun) =>
          val sTuple = STuple(
            args.map(MTermLoader(cInfo, _).get._2.get),
            MTypeLoader.fromTpt(tpt).get.asInstanceOf[StTuple]
          )
          Some(cInfo, Some(sTuple))
        case _ => None
      }
    }
  }
  object SAssignLoader extends Loader[SAssign] {
    def apply(cInfo: CircuitInfo, tr: Tree): Option[(CircuitInfo, Option[SAssign])] = {
      val (tree, _) = passThrough(tr)
      tree match {
        case Assign(lhs, rhs) => {
          val (newCInfo, left :: right :: Nil) = MTermLoader.loadTerms(cInfo, List(lhs, rhs))
          Some(newCInfo, Some(SAssign(left, right)))
        }
        case Apply(Select(qualifier, name), args) if name.toString().endsWith("_$eq") =>
          val leftName = name.toString().dropRight(4)
          val (newCInfo, left :: right :: Nil) = MTermLoader.loadTerms(
            cInfo,
            List(
              // this `TypeTree` only used for distinguish `SignalType` and other
              Typed(Select(qualifier, leftName), TypeTree(args.head.tpe)),
              args.head
            )
          )

          Some((newCInfo, Some(SAssign(left, right))))

        case _ => None
      }
    }
  }

  object SApplyLoader extends Loader[SApply] {
    def apply(cInfo: CircuitInfo, tr: Tree): Option[(CircuitInfo, Option[SApply])] = {
      val (tree, tpt) = passThrough(tr)
      tree match {
        case Apply(fun, args) =>
          val (newCInfo, (sTerm: STerm) :: mArgs) = MTermLoader.loadTerms(cInfo, fun :: args)
          val tpe                                 = MTypeLoader.fromTpt(tpt).get
          Some((cInfo, Some(SApply(sTerm, mArgs, tpe))))
        case _ => None
      }
    }
  }
}
