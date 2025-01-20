package chicala.convert.frontend

import scala.tools.nsc.Global

trait IdentsReader { self: Scala2Reader =>
  val global: Global
  import global._

  object IdentReader extends Loader[MTerm] {
    def apply(cInfo: CircuitInfo, tr: Tree): LREitherLoaded[MTerm] = {
      val (tree, tpt) = passThrough(tr)
      tree match {
        case i @ Ident(name: TermName) =>
          if (isChiselSignalType(i))
            Right(Loaded(cInfo, SignalRef(i, cInfo.getSignalType(i))))
          else {
            Right(Loaded(cInfo, SIdent(name, MTypeLoader.fromTpt(tpt).get)))
          }
        case _ =>
          unprocessedTree(tree, "IdentReader")
          Left(Failed)
      }

    }

  }

}
