package chicala.convert.frontend

import scala.tools.nsc.Global

trait ApplysReader { self: Scala2Reader =>
  val global: Global
  import global._

  object ApplyReader extends Reader[MTerm] {
    def apply(cInfo: CircuitInfo, tr: Tree): Either[LRError, Loaded[MTerm]] = {
      firstMatchIn(
        cInfo,
        tr,
        List(
          AssertLoader(_, _),
          WhenLoader(_, _),
          SwitchLoader(_, _),
          ConnectLoader(_, _),
          CApplyLoader(_, _),
          GenCTypeLoader(_, _),
          LitLoader(_, _),
          STupleLoader(_, _),
          SAssignLoader(_, _),
          SApplyLoader(_, _)
        ) // : List[(CircuitInfo, Tree) => Either[LRAllLeft, Loaded[MTerm]]]
      ).left.flatMap {
        case NotThis =>
          unprocessedTree(tr, "ApplyReader")
          Left(Failed)
        case x: LRError => Left(x)
      }
    }
  }
}
