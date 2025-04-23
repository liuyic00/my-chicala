package chicala.convert.pass

import scala.tools.nsc.Global

import chicala.ast.ChicalaAst

import chicala.ast.util.Transformers
import chicala.ast.util.InMStatements
import chicala.util.Printer

trait Sv2ChiselSimplifys extends ChicalaPasss with Transformers with InMStatements with Printer { self: ChicalaAst =>
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
                    _
                  ),
                  TermName("apply"),
                  StFunc
                ),
                List(l, r),
                StWrapped("sv2chisel.helpers.SubWords[_ <: chisel3.Data]")
              ) => {
            val tpe = a.tpe.asInstanceOf[Vec]
            // l + 1 == a.size && r == 0
            if (
              tpe.size == KnownSize(SApply(SSelect(l, TermName("$plus"), StFunc), List(SLiteral(1, StInt)), StInt)) &&
              r == SLiteral(0, StInt)
            ) {
              a
            } else {
              s
            }
          }
          // a.:= expr
          case SApply(
                SSelect(
                  a @ SApply(_, _, StWrapped("sv2chisel.helpers.SubWords[_ <: chisel3.Data]")),
                  TermName("$colon$eq"),
                  StFunc
                ),
                List(expr),
                StUnit
              ) => {
            val newA = transformMTerm(a)
            if (newA.tpe.isSignalType) {
              Connect(newA, transformMTerm(expr))
            } else {
              SApply(
                SSelect(
                  newA,
                  TermName("$colon$eq"),
                  StFunc
                ),
                List(expr),
                StUnit
              )
            }
          }

          case _ => super.transform(mStatement)
        }
      }

    }
  }
}
