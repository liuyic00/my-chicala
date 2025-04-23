package chicala.convert.frontend

import scala.tools.nsc.Global

trait FunctionsReader { self: Scala2Reader =>
  val global: Global
  import global._

  object FunctionReader extends Reader[SFunction] {
    def apply(cInfo: CircuitInfo, tr: Tree): Either[LRError, Loaded[SFunction]] = {
      val (tree, tpt) = passThrough(tr)
      tree match {
        case Function(vparams, body) =>
          StatementReader
            .fromListTree(cInfo, vparams)
            .asInstanceOf[Either[LRError, ModifiedAndLoaded[List[MValDef]]]]
            .flatMap { case ModifiedAndLoaded(newCInfo, vps) =>
              MTermLoader.must(newCInfo, body).map { case Loaded(b) =>
                Loaded(SFunction(vps, b))
              }
            }
        case _ =>
          unprocessedTree(tree, "FunctionReader")
          Left(Failed)
      }
    }

  }

}
