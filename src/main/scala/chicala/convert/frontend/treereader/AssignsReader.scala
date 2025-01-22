package chicala.convert.frontend

import scala.tools.nsc.Global

trait AssignsReader { self: Scala2Reader =>
  val global: Global
  import global._

  object AssignReader {
    def apply(cInfo: CircuitInfo, tr: Tree): LREitherT[ModifiedAndLoaded[SAssign]] = {
      SAssignLoader(cInfo, tr).flatMap {
        case value: ModifiedAndLoaded[_] =>
          Right(value)
        case Modified(_) =>
          errorTree(tr, "AssignReader")
          Left(Failed)
        case NotThis =>
          unprocessedTree(tr, "AssignReader")
          Left(Failed)
      }
    }
  }
}
