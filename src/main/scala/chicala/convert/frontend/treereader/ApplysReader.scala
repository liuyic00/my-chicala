package chicala.convert.frontend

import scala.tools.nsc.Global

trait ApplysReader { self: Scala2Reader =>
  val global: Global
  import global._

  object ApplyReader {
    def apply(cInfo: CircuitInfo, tr: Tree): LREitherT[ModifiedAndLoaded[MTerm]] = {
      firstMatchIn[MTerm](
        cInfo,
        tr,
        List(
          AssertLoader(_, _),
          WhenLoader(_, _),
          SwitchLoader(_, _),
          ConnectLoader(_, _),
          CApplyLoader(_, _),
          LitLoader(_, _),
          STupleLoader(_, _),
          SAssignLoader(_, _),
          SApplyLoader(_, _)
        )
      ).flatMap {
        case value: ModifiedAndLoaded[_] =>
          Right(value)
        case NotThis =>
          unprocessedTree(tr, "ApplyReader")
          Left(Failed)
        case Modified(cInfo) =>
          errorTree(tr, "ApplyReader")
          Left(Failed)
      }
    }
  }
}
