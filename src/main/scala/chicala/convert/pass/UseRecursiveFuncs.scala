package chicala.convert.pass

import scala.tools.nsc.Global

import chicala.ast.ChicalaAst

import chicala.ast.util.{Transformers, Computes}
import chicala.ast.util.InMStatements
import chicala.util.Printer
import chicala.ChicalaConfig

trait UseRecursiveFuncs extends ChicalaPasss with Transformers { self: ChicalaAst =>
  val global: Global
  import global._

  object UseRecursiveFunc extends ChicalaPass {
    def apply(cClassDef: CClassDef): CClassDef = {
      if (ChicalaConfig.useRecursiveFunc) {
        val useRecursiveFunc = new UseRecursiveFunc
        cClassDef match {
          case m: ModuleDef => m.copy(body = m.body.flatMap(useRecursiveFunc.transformToMuti(_)))
          case b: BundleDef => b
        }
      } else
        cClassDef
    }

    class UseRecursiveFunc extends Transformer {
      var tmpFuncNameIndex = 0
      def getRecFunc(mStatement: MStatement): Option[List[MStatement]] = mStatement match {
        case s @ SApply(
              SSelect(
                SApply(
                  SSelect(
                    SApply(_, List(a), StWrapped("scala.runtime.RichInt")),
                    TermName("until"),
                    StFunc
                  ),
                  List(b),
                  StWrapped("scala.collection.immutable.Range")
                ),
                TermName("foreach"),
                StFunc
              ),
              List(SFunction(List(SValDef(iTermName, StInt, EmptyMTerm, false)), funcp)),
              _
            ) =>
          // a until b foreach (i => { ... })
          tmpFuncNameIndex += 1
          val tmpFuncName = TermName(s"tmpFunc${tmpFuncNameIndex}")
          val i           = SIdent(iTermName, StInt)
          Some(
            List(
              SDefDef(
                tmpFuncName,
                List(List(SValDef(iTermName, StInt, EmptyMTerm, false))),
                StUnit,
                funcp.append(
                  SIf(
                    // i < b - 1
                    SApply(
                      SSelect(i, TermName("$less"), StFunc),
                      List(SApply(SSelect(b, TermName("$minus"), StFunc), List(SLiteral(1, StInt)), StInt)),
                      StBoolean
                    ),
                    // tmpFuncName(i + 1)
                    SApply(
                      SIdent(tmpFuncName, StFunc),
                      List(SApply(SSelect(i, TermName("$plus"), StFunc), List(SLiteral(1, StInt)), StInt)),
                      StUnit
                    ),
                    EmptyMTerm,
                    StUnit
                  )
                )
              ),
              SApply(SIdent(tmpFuncName, StFunc), List(a), StUnit)
            )
          )
        case _ => None
      }
      def transformToMuti(mStatement: MStatement): List[MStatement] = {
        getRecFunc(mStatement) match {
          case Some(list) => list
          case None       => List(super.transform(mStatement))
        }
      }
      override def transform(mStatement: MStatement): MStatement = mStatement match {
        case SBlock(body, tpe) =>
          SBlock(
            body.flatMap(transformToMuti),
            tpe
          )
        case _ =>
          getRecFunc(mStatement) match {
            case Some(list) => SBlock(list, StUnit)
            case None       => super.transform(mStatement)
          }

      }
    }
  }

}
