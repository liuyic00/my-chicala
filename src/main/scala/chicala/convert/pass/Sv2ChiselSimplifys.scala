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
              SLib("chicala.lib.helper.BoolVec.Fill", StFunc),
              List(n, funcp),
              Vec(KnownSize(n), Node, Bool(Node, Undirect))
            )
          case SApply(
                SSelect(
                  SApply(
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
                    _
                  ),
                  TermName("$colon$eq"),
                  StFunc
                ),
                List(expr),
                _
              ) => {
            val tpe = a.tpe.asInstanceOf[Vec]
            val left =
              if (
                tpe.size == KnownSize(SApply(SSelect(l, TermName("$plus"), StFunc), List(SLiteral(1, StInt)), StInt)) &&
                r == SLiteral(0, StInt)
              ) {
                // l + 1 = a.size && r = 0
                a
              } else {
                SApply(
                  SLib("chicala.lib.helper.BoolVec.Slice", StFunc),
                  List(transformStatementT(a), transformStatementT(l), transformStatementT(r)),
                  tpe.copy(size =
                    KnownSize(
                      SApply(
                        SSelect(
                          SApply(
                            SSelect(l, TermName("$plus"), StFunc),
                            List(SLiteral(1, StInt)),
                            StInt
                          ),
                          TermName("$minus"),
                          StFunc
                        ),
                        List(r),
                        StInt
                      )
                    )
                  )
                )
              }
            Connect(left, transformStatementT(expr))
          }

          case _ => super.transform(mStatement)
        }
      }

    }
  }
}
