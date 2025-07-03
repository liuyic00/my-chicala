package chicala.convert.pass

import scala.tools.nsc.Global

import chicala.ast.ChicalaAst

import chicala.ast.util.Transformers
import chicala.ast.util.InMStatements
import chicala.util.Printer
import chicala.ChicalaConfig

trait ReduceAsTypeOfs extends ChicalaPasss with Transformers { self: ChicalaAst =>
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
          case CApply(AsTypeOf, List(Lit(SLiteral(0, StInt), UInt(_, _, _)), b)) if b.tpe.isSignalType =>
            GenCType(b.tpe.asInstanceOf[SignalType])
          case x => super.transform(x)
        }
      }
    }

  }
}
