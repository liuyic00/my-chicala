package chicala.convert.pass

import scala.tools.nsc.Global

import chicala.ast.ChicalaAst

import chicala.ast.util.Transformers
import chicala.ast.util.InMStatements
import chicala.util.Printer

trait BeforeEmitScalas extends ChicalaPasss with Transformers { self: ChicalaAst =>
  val global: Global
  import global._

  object BeforeEmitScala extends ChicalaPass {
    def apply(cClassDef: CClassDef): CClassDef = {
      cClassDef match {
        case m: ModuleDef =>
          val nameCheck = new NameCheck(m.name)
          m.copy(body =
            m.body.map(s =>
              List(
                // SubModuleRun in ModuleDef body do not need SBlock
                subModuleRunWrapInSBlock.superTransform(_),
                nameCheck(_)
              ).foldLeft(s) { (s, f) => f(s) }
            )
          )
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

    /** Check and modify variable names */
    class NameCheck(moduleName: TypeName) extends Transformer {
      var replaceMap: Map[String, Tree] = Map.empty
      override def transform(mStatement: MStatement): MStatement = {
        mStatement match {
          case SubModuleDef(name, tpe, args) => SubModuleDef(name, tpe, args)
          case SubModuleRun(name, inputRefs, outputRefs, moduleType, inputIos, outputIos) =>
            SubModuleRun(
              name,
              inputRefs,
              outputRefs,
              moduleType,
              inputIos,
              outputIos.map { case (name, tpe) =>
                (name.head.toLower +: name.tail, tpe)
              }
            )
          // TODO: add RegDef
          case IoDef(name, tpe) =>
            if (name.toString().head.isLower) {
              IoDef(name, tpe)
            } else {
              val newName = TermName(name.toString().head.toLower +: name.toString().tail)
              replaceMap += (Select(This(moduleName), name).toString() -> Select(This(moduleName), newName))
              IoDef(newName, tpe)
            }
          case _ => super.transform(mStatement)
        }
      }
      override def transformTree(tree: Tree): Tree = {
        replaceMap.getOrElse(tree.toString(), super.transformTree(tree))
      }
    }
  }
}
