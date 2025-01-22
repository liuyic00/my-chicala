package chicala.convert.frontend

import scala.tools.nsc.Global

trait BlocksReader { self: Scala2Reader =>
  val global: Global
  import global._

  object BlockReader {
    def apply(cInfo: CircuitInfo, tr: Tree): LREitherT[ModifiedAndLoaded[SBlock]] = {
      val (tree, tpt) = passThrough(tr)
      tree match {
        case Block(stats, expr) =>
          StatementReader
            .fromListTree(cInfo, stats :+ expr)
            .map { case ModifiedAndLoaded(newCInfo, cList) =>
              // Block not influence outside cInfo
              ModifiedAndLoaded(cInfo, SBlock(cList, EmptyMType))
            }
        case _ =>
          unprocessedTree(tree, "BlockReader")
          Left(Failed)
      }

    }

  }

}
