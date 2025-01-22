package chicala.convert.frontend

import scala.tools.nsc.Global

trait StatementsReader { self: Scala2Reader =>
  val global: Global
  import global._

  object StatementReader extends Loader[MStatement] {
    def fromListTree(cInfo: CircuitInfo, body: List[Tree]): LREitherT[ModifiedAndLoaded[List[MStatement]]] = {
      logger.in("StatementReader.fromListTree")
      val a = body
        .foldLeft(Right(ModifiedAndLoaded(cInfo, List.empty)): Either[LRExit, ModifiedAndLoaded[List[MStatement]]]) {
          case (Right(ModifiedAndLoaded(info, revPast)), tr) =>
            StatementReader(info, tr) match {
              case Right(ModifiedAndLoaded(newCInfo, newStat)) => Right(ModifiedAndLoaded(newCInfo, newStat :: revPast))
              case Right(Modified(newCInfo))                   => Right(ModifiedAndLoaded(newCInfo, revPast))
              case Left(x: LRSkip)                             => Right(ModifiedAndLoaded(info, revPast))
              case Left(x: LRExit)                             => Left(x)
            }
          case (Left(x: LRExit), _) => Left(x)
        }
        .map(_.mapValue(_.reverse))
      logger.out("StatementReader.fromListTree")
      a
    }

    def apply(cInfo: CircuitInfo, tr: Tree): LREitherT[LRSuccess[MStatement]] = {
      val (tree, tpt) = passThrough(tr)
      tree match {
        case _: ValDef | _: DefDef => MDefLoader(cInfo, tr)
        case _                     => MTermLoader(cInfo, tr)
      }
    }
  }

}
