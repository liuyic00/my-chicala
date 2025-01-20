package chicala.convert.frontend

import scala.tools.nsc.Global

trait IfsReader { self: Scala2Reader =>
  val global: Global
  import global._

  object IfReader extends Loader[SIf] {
    def apply(cInfo: CircuitInfo, tr: Tree): LREitherLoaded[SIf] = {
      val (tree, tpt) = passThrough(tr)
      tree match {
        case i @ If(cond, thenp, elsep) => {
          MTermLoader.loadTerms(cInfo, List(cond, thenp, elsep)).flatMap {
            case Loaded(newCInfo, (c: STerm) :: t :: e :: Nil) =>
              val tpe = MTypeLoader.fromTpt(tpt).get
              Right(Loaded(newCInfo, SIf(c, t, e, tpe)))
            case _ => loadMutilpleMatchError(i)
          }
        }
        case _ =>
          unprocessedTree(tree, "IfsReader")
          Left(Failed)
      }
    }
  }
}
