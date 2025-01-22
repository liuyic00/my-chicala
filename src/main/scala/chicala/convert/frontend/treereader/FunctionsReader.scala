package chicala.convert.frontend

import scala.tools.nsc.Global

trait FunctionsReader { self: Scala2Reader =>
  val global: Global
  import global._

  object FunctionReader extends Loader[SFunction] {
    def apply(cInfo: CircuitInfo, tr: Tree): LREitherT[ModifiedAndLoaded[SFunction]] = {
      val (tree, tpt) = passThrough(tr)
      tree match {
        case Function(vparams, body) =>
          StatementReader
            .fromListTree(cInfo, vparams)
            .asInstanceOf[LREitherT[ModifiedAndLoaded[List[MValDef]]]]
            .flatMap { case ModifiedAndLoaded(newCInfo, vps) =>
              MTermLoader(newCInfo, body).map { case ModifiedAndLoaded(_, b) =>
                ModifiedAndLoaded(cInfo, SFunction(vps, b))
              }
            }
        case _ =>
          unprocessedTree(tree, "FunctionReader")
          Left(Failed)
      }
    }

  }

}
