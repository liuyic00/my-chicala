package chicala.convert.frontend

import scala.tools.nsc.Global

trait BlocksReader { self: Scala2Reader =>
  val global: Global
  import global._

  object BlockReader {
    def apply(cInfo: CircuitInfo, tr: Tree): LREitherLoaded[SBlock] = {
      val (tree, tpt) = passThrough(tr)
      tree match {
        case Block(stats, expr) =>
          StatementReader
            .fromListTree(cInfo, stats :+ expr)
            .map { case Loaded(newCInfo, cList) =>
              // Block not influence outside cInfo
              Loaded(cInfo, SBlock(cList, EmptyMType))
            }
        case _ =>
          unprocessedTree(tree, "BlockReader")
          Left(Failed)
      }

    }

  }

}
