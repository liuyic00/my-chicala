package chicala.convert.frontend

import scala.tools.nsc.Global

trait LiteralsReader { self: Scala2Reader =>
  val global: Global
  import global._

  object LiteralReader extends Reader[MTerm] {
    def apply(cInfo: CircuitInfo, tr: Tree): Either[LRError, Loaded[MTerm]] = {
      val (tree, tpt) = passThrough(tr)
      tree match {
        case Literal(Constant(())) =>
          Right(Loaded(EmptyMTerm))
        case l @ Literal(Constant(value)) =>
          Right(Loaded(SLiteral(value, STypeLoader.fromTpt(tpt).get)))
        case _ =>
          unprocessedTree(tree, "LiteralReader")
          Left(Failed)
      }

    }

  }

}
