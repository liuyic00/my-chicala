package chicala.convert.pass

import chicala.ast.ChicalaAst

trait ChicalaPasss { self: ChicalaAst =>

  trait ChicalaPass {
    def apply(cClassDef: CClassDef): CClassDef
  }

  object RunChicalaPass {
    def apply(cClassDef: CClassDef, passs: List[ChicalaPass]): CClassDef = {
      passs.foldLeft(cClassDef)((a, b) => b(a))
    }
  }
}

trait ChicalaPassCollecttion
    extends DependencySorts
    with AfterSorts
    with BeforeEmitScalas
    with ChicalaPeeks
    with ExpandSubModuleDefs
    with LiteralPropagations
    with ReduceAsTypeOfs
    with RegEnableApplys
    with Sv2ChiselSimplifys
    with UseNestedCats
    with UseRecursiveFuncs
    with UseVecOnlys {
  self: ChicalaAst =>
}
