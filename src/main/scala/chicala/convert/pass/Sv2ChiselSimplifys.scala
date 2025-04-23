package chicala.convert.pass

import scala.tools.nsc.Global

import chicala.ast.ChicalaAst

import chicala.ast.util.{Transformers, Computes}
import chicala.ast.util.InMStatements
import chicala.util.Printer

trait Sv2ChiselSimplifys extends ChicalaPasss with Transformers with InMStatements with Printer with Computes {
  self: ChicalaAst =>
  val global: Global
  import global._

  object Sv2ChiselSimplify extends ChicalaPass {
    def apply(cClassDef: CClassDef): CClassDef = {
      cClassDef match {
        case m: ModuleDef =>
          m.copy(body = m.body.map(sv2ChiselSimplifyTransformer(_)))
        case b: BundleDef => b
      }
    }

    object sv2ChiselSimplifyTransformer extends Transformer {
      override def transform(mStatement: MStatement): MStatement = {
        mStatement match {
          // VecInit.tabulate(n)(vp => funcp)
          case SApply(
                SApply(SLib("chisel3.VecInit.do_tabulate", StFunc), List(n: STerm), StFunc),
                List(SFunction(List(SValDef(vp, _, _, _)), funcp)),
                Vec(_, _, _: Bool)
              ) if (!InMStatementHasTermName(funcp, vp)) =>
            SApply(
              SLib("h.bv.Fill", StFunc),
              List(n, funcp),
              Vec(KnownSize(n), Node, Bool(Node, Undirect))
            )
          // a(l,r)
          case s @ SApply(
                SSelect(
                  SApply(
                    SLib("sv2chisel.helpers.vecconvert.`package`.vecToSubwords", StFunc),
                    List(a),
                    StWrapped("sv2chisel.helpers.SubWordable[_ <: chisel3.Data]")
                  ),
                  TermName("apply"),
                  StFunc
                ),
                List(l: STerm, r: STerm),
                StWrapped("sv2chisel.helpers.SubWords[_ <: chisel3.Data]")
              ) => {
            val tpe = a.tpe.asInstanceOf[Vec]
            // l + 1 == a.size && r == 0
            tpe.size match {
              case KnownSize(size) =>
                if (
                  isEqual(size, plus(l, SLiteral(1, StInt))) &&
                  r == SLiteral(0, StInt)
                ) a
                else
                  CApply(Slice, List(a, l, r))
              case _ =>
                CApply(Slice, List(a, l, r))
            }
          }
          // subwordsToVec(a)
          case SApply(
                SLib("sv2chisel.helpers.vecconvert.`package`.subwordsToVec", StFunc),
                List(a),
                tpe: Vec
              ) => {
            transformMTerm(a)
          }
          // a(l,r).termName(expr)
          case SApply(
                SSelect(
                  a @ SApply(_, _, StWrapped("sv2chisel.helpers.SubWords[_ <: chisel3.Data]")),
                  termName,
                  StFunc
                ),
                List(expr),
                sApplyType
              ) => {
            val newA    = transformMTerm(a)
            val newExpr = transformMTerm(expr)
            if (newA.tpe.isSignalType) {
              termName match {
                case TermName("$colon$eq") =>
                  Connect(newA, newExpr)
                case TermName("asTypeOf") =>
                  CApply(AsTypeOf, List(newA, newExpr))
              }
            } else {
              SApply(
                SSelect(newA, termName, StFunc),
                List(newExpr),
                sApplyType
              )
            }
          }

          case _ => super.transform(mStatement)
        }
      }

    }
  }
}
