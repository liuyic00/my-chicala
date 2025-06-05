package chicala.convert.pass

import scala.tools.nsc.Global

import chicala.ast.ChicalaAst

import chicala.ast.util.{Transformers, Computes}
import chicala.ast.util.InMStatements
import chicala.util.Printer

trait BeforEmitScalas extends ChicalaPasss with Transformers { self: ChicalaAst =>
  val global: Global
  import global._

  object BeforEmitScala extends ChicalaPass {
    def apply(cClassDef: CClassDef): CClassDef = {
      cClassDef match {
        case m: ModuleDef =>
          // SubModuleRun in ModuleDef body do not need SBlock
          m.copy(body = m.body.map(subModuleRunWrapInSBlock.superTransform(_)))
        case b: BundleDef => b
      }
    }

    /** SubModuleRun will emit multiple statements in Scala, so we need to wrap
      * it in SBlock.
      */
    object subModuleRunWrapInSBlock extends Transformer {
      def superTransform(mStatement: MStatement): MStatement = {
        super.transform(mStatement)
      }
      override def transform(mStatement: MStatement): MStatement = {
        mStatement match {
          // add SBlock for SubModuleRun
          case x: SubModuleRun => SBlock(List(x), StUnit)
          // skip if already SBlock
          case SBlock(body, tpe) => SBlock(body.map(super.transform(_)), tpe)
          case _                 => super.transform(mStatement)
        }
      }

    }
  }
}
