package chicala.convert.frontend

import scala.tools.nsc.Global

trait LiteralsReader { self: Scala2Reader =>
  val global: Global
  import global._

  object LiteralReader {
    def apply(cInfo: CircuitInfo, tr: Tree): LREitherLoaded[MTerm] = {
      val (tree, tpt) = passThrough(tr)
      tree match {
        case Literal(Constant(())) =>
          Right(Loaded(cInfo, EmptyMTerm))
        case l @ Literal(Constant(value)) =>
          Right(Loaded(cInfo, SLiteral(value, STypeLoader.fromTpt(tpt).get)))
        case _ =>
          unprocessedTree(tree, "LiteralReader")
          Left(Failed)
      }

    }

  }

}
