package chicala.convert.frontend

import scala.tools.nsc.Global

trait ReaderInfos { this: Scala2Reader =>
  val global: Global
  import global._

  case class ProcessLater(
      tree: Tree,
      packageName: String,
      fullName: String,
      depenName: String
  )

  case class ReaderInfo(
      moduleDefs: Map[String, ModuleDef],
      bundleDefs: Map[String, BundleDef],
      todos: List[ProcessLater],
      dependentClassNotDef: Boolean
  ) {
    def settedDependentClassNotDef  = copy(dependentClassNotDef = true)
    def clearedDependentClassNotDef = copy(dependentClassNotDef = false)
    def isDependentClassNotDef      = dependentClassNotDef

    def addedModuleDef(moduleDef: ModuleDef) =
      copy(moduleDefs = moduleDefs + (moduleDef.fullName -> moduleDef))
    def addedBundleDef(bundleDef: BundleDef) =
      copy(bundleDefs = bundleDefs + (bundleDef.fullName -> bundleDef))
    def addedTodo(p: ProcessLater) =
      copy(todos = todos.appended(p))

    def needExit = isDependentClassNotDef
  }

  object ReaderInfo {
    def empty = ReaderInfo(Map.empty, Map.empty, List.empty, false)
  }

}
