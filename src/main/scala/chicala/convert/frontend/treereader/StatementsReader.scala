package chicala.convert.frontend

import scala.tools.nsc.Global

trait StatementsReader { self: Scala2Reader =>
  val global: Global
  import global._

  object StatementReader extends Reader[MStatement] {
    def fromListTree(cInfo: CircuitInfo, body: List[Tree]): Either[LRError, ModifiedAndLoaded[List[MStatement]]] = {
      logger.in("StatementReader.fromListTree")
      val a = body
        .foldLeft(Right(ModifiedAndLoaded(cInfo, List.empty)): Either[LRExit, ModifiedAndLoaded[List[MStatement]]]) {
          case (Right(ModifiedAndLoaded(info, revPast)), tr) => {
            StatementReader(info, tr)
              .map({
                case ModifiedAndLoaded(newCInfo, newStat) => ModifiedAndLoaded(newCInfo, newStat :: revPast)
                case Modified(newCInfo)                   => ModifiedAndLoaded(newCInfo, revPast)
                case Loaded(newStat)                      => ModifiedAndLoaded(info, newStat :: revPast)
              })
              .left
              .flatMap({
                case x: LRSkip => Right(ModifiedAndLoaded(info, revPast))
                case x: LRExit => Left(x)
              })
          }
          case (Left(x), _) => Left(x)
        }
        .map(_.mapValue(_.reverse))
      logger.out("StatementReader.fromListTree")
      a
    }

    def apply(cInfo: CircuitInfo, tr: Tree): Either[LRError, LRSuccess[MStatement]] = {
      val (tree, tpt) = passThrough(tr)
      tree match {
        case _: ValDef | _: DefDef => MDefLoader(cInfo, tr)
        case _                     => MTermLoader(cInfo, tr)
      }
    }
  }

}
