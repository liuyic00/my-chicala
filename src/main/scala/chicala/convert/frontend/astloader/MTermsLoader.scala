package chicala.convert.frontend

import scala.tools.nsc.Global

trait MTermsLoader { self: Scala2Reader =>
  val global: Global
  import global._

  object MTermLoader extends LoadedLoader[MTerm] {
    def apply(cInfo: CircuitInfo, tr: Tree): Either[LRAllLeft, Loaded[MTerm]] = {
      val (tree, tpt) = passThrough(tr)
      tree match {
        case _: Apply    => ApplyReader(cInfo, tr)
        case _: Block    => BlockReader(cInfo, tr)
        case _: Ident    => IdentReader(cInfo, tr)
        case _: Literal  => LiteralReader(cInfo, tr)
        case _: Select   => SelectReader(cInfo, tr)
        case _: If       => IfReader(cInfo, tr)
        case _: Function => FunctionReader(cInfo, tr)
        case _: Match    => MatchReader(cInfo, tr)
        case _: Assign   => AssignReader(cInfo, tr)
        case EmptyTree   => Right(Loaded(EmptyMTerm))
        case _           => Left(NotThis)
      }
    }

    def loadTerms(cInfo: CircuitInfo, trees: List[Tree]): Either[LRError, Loaded[List[MTerm]]] = {
      loadMutilple(cInfo)(trees.map(x => (y => MTermLoader.must(y, x))): _*)
    }

  }

}
