package chicala.convert.frontend

import scala.tools.nsc.Global

trait FunctionsReader { self: Scala2Reader =>
  val global: Global
  import global._

  object FunctionReader extends Loader[SFunction] {
    def apply(cInfo: CircuitInfo, tr: Tree): LREitherLoaded[SFunction] = {
      val (tree, tpt) = passThrough(tr)
      tree match {
        case Function(vparams, body) =>
          StatementReader
            .fromListTree(cInfo, vparams)
            .asInstanceOf[LREitherLoaded[List[MValDef]]]
            .flatMap { case Loaded(newCInfo, vps) =>
              MTermLoader(newCInfo, body).map(_.flatMap { b =>
                Loaded(cInfo, SFunction(vps, b))
              })
            }
        case _ =>
          unprocessedTree(tree, "FunctionReader")
          Left(Failed)
      }
    }

  }

}
