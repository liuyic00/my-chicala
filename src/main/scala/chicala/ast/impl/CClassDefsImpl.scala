package chicala.ast.impl

import scala.tools.nsc.Global

import chicala.ast.ChicalaAst
import chicala.ast.util.Replacers

trait CClassDefsImpl extends Replacers { self: ChicalaAst =>
  val global: Global
  import global._

  trait ModuleDefImpl { self: ModuleDef =>
    def ioDefs: List[IoDef] = {
      val ios        = body.collect({ case x: IoDef => x })
      val sValDefs   = body.collect({ case x: SValDef => x })
      val replaceMap = sValDefs.map(x => (SIdent(x.name, x.tpe): MStatement) -> x.rhs).toMap
      val replacer   = Replacer(replaceMap).convergence
      val newIOs     = ios.map(replacer.transformT(_))
      newIOs.isEmpty match {
        case true =>
          reportError(NoPosition, "ModuleDef should has a IoDef in body")
          List(IoDef(TermName(""), SignalType.empty))
        case false => newIOs
      }
    }
    def regDefs: List[RegDef] = body.collect { case x: RegDef => x }
  }
  trait BundleDefImpl { self: BundleDef =>
    def applyArgs(args: List[MTerm]): BundleDef = {
      val replaceMap: Map[MStatement, MStatement] = vparams
        .zip(args)
        .map({ case (p, a) => SIdent(p.name, p.tpe) -> a })
        .toMap

      this.copy(bundle = Replacer(replaceMap).transformTypeT(bundle))
    }
  }
}
