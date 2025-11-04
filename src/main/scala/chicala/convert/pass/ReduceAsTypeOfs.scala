package chicala.convert.pass

import scala.tools.nsc.Global

import chicala.ast.ChicalaAst
import chicala.ast.util.{Compares, Transformers}

trait ReduceAsTypeOfs extends ChicalaPasss with Transformers with Compares { self: ChicalaAst =>
  val global: Global
  import global._

  object ReduceAsTypeOf extends ChicalaPass {
    def apply(cClassDef: CClassDef): CClassDef = {
      cClassDef match {
        case m: ModuleDef => m.copy(body = m.body.map(reduceAsTypeOf(_)))
        case b: BundleDef => b
      }
    }

    object reduceAsTypeOf extends Transformer {
      override def transform(mStatement: MStatement): MStatement = {
        mStatement match {
          // AsTypeOf
          case CApply(
                AsTypeOf,
                List(
                  Lit(SLiteral(0, StInt), _: UInt) | SApply(SLib("h.bv.Lit", StFunc), List(SLiteral(0, StInt)), _),
                  b
                )
              ) if b.tpe.isSignalType =>
            transform(GenCType(b.tpe.asInstanceOf[SignalType]))
          case CApply(AsTypeOf, List(op1, op2)) =>
            val newOp1 = transformT(op1)
            val newOp2 = transformT(op2)
            if (sameKnownSignalType(newOp1.tpe, newOp2.tpe))
              newOp1
            else {
              (newOp1.tpe, newOp2.tpe) match {
                case (Vec(_, _, Bool(_, _)), Vec(KnownSize(size), _, Bool(_, _))) =>
                  CApply(VecTake, List(newOp1, size))
                case (
                      Vec(_, _, Bool(_, _)),
                      t2 @ Vec(KnownSize(size1), _, Vec(KnownSize(size2), _, Bool(_, _)))
                    ) =>
                  SApply(
                    SLib("h.bv.list2listlist", StFunc),
                    List(newOp1, size1, size2),
                    t2.nomalize
                  )
                case (
                      Vec(_, _, Vec(_, _, Bool(_, _))),
                      t2 @ Vec(KnownSize(size), _, Bool(_, _))
                    ) =>
                  SApply(
                    SLib("h.bv.listlist2list", StFunc),
                    List(newOp1, size),
                    t2.nomalize
                  )
                case _ =>
                  CApply(AsTypeOf, List(newOp1, newOp2))
              }
            }
          case Connect(left, expr) => {
            val newLeft = transformT(left)
            val newExpr = transformT(expr)
            (newLeft.tpe, newExpr.tpe) match {
              case (t1 @ Vec(KnownSize(size), _, Bool(_, _)), t2 @ Vec(_, _, Bool(_, _)))
                  if !sameKnownSignalType(t1, t2) =>
                Connect(newLeft, CApply(VecTake, List(newExpr, size)))
              case _ =>
                Connect(newLeft, newExpr)
            }
          }
          case x => super.transform(x)
        }
      }
    }

  }
}
