package chicala.convert.frontend

import scala.tools.nsc.Global

trait StatementsReader { self: Scala2Reader =>
  val global: Global
  import global._

  object StatementReader extends Loader[MStatement] {
    def fromListTree(cInfo: CircuitInfo, body: List[Tree]): LREitherLoaded[List[MStatement]] = {
      logger.in("StatementReader.fromListTree")
      val a = body
        .foldLeft(Right(Loaded(cInfo, List.empty)): Either[LRExit, Loaded[List[MStatement]]]) {
          case (Right(Loaded(info, revPast)), tr) =>
            StatementReader(info, tr) match {
              case Right(Loaded(newCInfo, newStat)) => Right(Loaded(newCInfo, newStat :: revPast))
              case Right(Changed(newCInfo))         => Right(Loaded(newCInfo, revPast))
              case Left(x: LRSkip)                  => Right(Loaded(info, revPast))
              case Left(x: LRExit)                  => Left(x)
            }
          case (Left(x: LRExit), _) => Left(x)
        }
        .map(_.map(_.reverse))
      logger.out("StatementReader.fromListTree")
      a
    }

    def apply(cInfo: CircuitInfo, tr: Tree): LREitherSuccess[MStatement] = {
      val (tree, tpt) = passThrough(tr)
      tree match {
        case _: ValDef | _: DefDef => MDefLoader(cInfo, tr)
        case _                     => MTermLoader(cInfo, tr)
      }
    }
  }

}
