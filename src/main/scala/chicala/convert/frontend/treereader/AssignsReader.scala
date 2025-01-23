package chicala.convert.frontend

import scala.tools.nsc.Global

trait AssignsReader { self: Scala2Reader =>
  val global: Global
  import global._

  object AssignReader extends Reader[SAssign] {
    def apply(cInfo: CircuitInfo, tr: Tree): Either[LRError, Loaded[SAssign]] = {
      SAssignLoader.must(cInfo, tr)
    }
  }
}
