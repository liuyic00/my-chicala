package chicala.convert.pass

import scala.tools.nsc.Global

import chicala.ast.ChicalaAst

import chicala.ast.util.{Transformers, Computes}
import chicala.ast.util.InMStatements
import chicala.util.Printer

trait AfterSorts extends ChicalaPasss with Transformers { self: ChicalaAst =>
  val global: Global
  import global._

  object AfterSort extends ChicalaPass {
    def apply(cClassDef: CClassDef): CClassDef = {
      cClassDef match {
        case m: ModuleDef =>
          m.copy(body =
            m.body.map(s =>
              List(
                // add needcheck comment
                addNeedCheckComment(_)
              ).foldLeft(s) { (s, f) => f(s) }
            )
          )
        case b: BundleDef => b
      }
    }

    /** Add NEEDCHECK comment */
    object addNeedCheckComment extends Transformer {
      override def transform(mStatement: MStatement): MStatement = {
        mStatement match {
          case s @ SApply(_, List(f: SFunction), _) => {
            val dependencys = s.relatedIdents.dependency
            val fullys      = s.relatedIdents.fully
            val intersect   = dependencys.intersect(fullys)
            val newS = if (intersect.nonEmpty) {
              val comment = Comment("chicala[NEEDCHECK]: function has self dependency inside")
              val newFuncp = f.funcp match {
                case SBlock(body, tpe) => SBlock(comment :: body, tpe)
                case x                 => SBlock(comment :: List(x), x.tpe)
              }
              s.copy(args = List(f.copy(funcp = newFuncp)))
            } else s

            super.transform(newS)
          }
          case _ => super.transform(mStatement)
        }
      }

    }

  }
}
