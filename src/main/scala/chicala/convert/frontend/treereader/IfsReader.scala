package chicala.convert.frontend

import scala.tools.nsc.Global

trait IfsReader { self: Scala2Reader =>
  val global: Global
  import global._

  object IfReader {
    def apply(cInfo: CircuitInfo, tr: Tree): Option[(CircuitInfo, Option[SIf])] = {
      val (tree, tpt) = passThrough(tr)
      tree match {
        case i @ If(cond, thenp, elsep) => {
          val (newCInfo, (c: STerm) :: t :: e :: Nil) = MTermLoader.loadTerms(cInfo, List(cond, thenp, elsep))
          val tpe                                     = MTypeLoader.fromTpt(tpt).get
          Some((newCInfo, Some(SIf(c, t, e, tpe))))
        }
        case _ =>
          unprocessedTree(tree, "IfsReader")
          None
      }
    }
  }
}
