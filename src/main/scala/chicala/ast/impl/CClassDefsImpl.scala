package chicala.ast.impl

import scala.tools.nsc.Global

import chicala.ast.ChicalaAst
import chicala.ast.util.Replacers

trait CClassDefsImpl extends Replacers { self: ChicalaAst =>
  val global: Global
  import global._

  trait ModuleDefImpl { self: ModuleDef =>
    private val localValReplacer = {
      val sValDefs   = body.collect({ case x: SValDef => x })
      val replaceMap = sValDefs.map(x => (SIdent(x.name, x.tpe): MStatement) -> x.rhs).toMap
      Replacer(replaceMap).convergence
    }
    def ioDefs: List[IoDef] = {
      val ios    = body.collect({ case x: IoDef => x })
      val newIOs = ios.map(localValReplacer.transformT(_))
      newIOs.isEmpty match {
        case true =>
          reportError(NoPosition, "ModuleDef should has a IoDef in body")
          List(IoDef(TermName(""), SignalType.empty))
        case false => newIOs
      }
    }
    def collectedRegDefs: List[RegDef] = {
      // TODO: deep into if-else branches
      body.collect {
        case x: RegDef =>
          List(x)
            .map(localValReplacer.transformT(_))
        case sub: SubModuleDef =>
          sub.tpe.collectedRegDefs
            .map(x => x.copy(name = TermName(s"${sub.name}_${x.name}")))
            .map(localValReplacer.transformT(_))
            .map(sub.tpe.argsReplacer(sub.args).transformT(_))
      }.flatten
    }
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
