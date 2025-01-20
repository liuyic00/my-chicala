package chicala.convert.frontend

import scala.tools.nsc.Global

trait MatchsReader { self: Scala2Reader =>
  val global: Global
  import global._

  object MatchReader {
    def apply(cInfo: CircuitInfo, tr: Tree): LREitherLoaded[SMatch] = {
      val (tree, tpt) = passThrough(tr)
      tree match {
        case Match(selector, cases) =>
          MTermLoader(cInfo, selector).flatMap { case Loaded(tcInfo, mTerm) =>
            val tpe = MTypeLoader.fromTpt(tpt).get

            val cs = loadMutilple(cInfo)(cases.map({ case CaseDef(pat, guard, body) =>
              (info: CircuitInfo) => {
                val nameTypes = pat match {
                  case Apply(t: TypeTree, args) =>
                    // assume `t` is some tuple
                    args.map { case b @ Bind(name: TermName, body) =>
                      (name, MTypeLoader.fromTpt(TypeTree(b.tpe)).get)
                    }
                  case Ident(termNames.WILDCARD) =>
                    // _ => ...
                    List((termNames.WILDCARD, StAny))
                  case x =>
                    unprocessedTree(x, "MatchReader cases")
                    List.empty
                }
                val newCInfo = nameTypes.foldLeft(info) { case (cf, (name, mType)) =>
                  cf.updatedVal(name, mType)
                }
                if (guard != EmptyTree) { unprocessedTree(guard, "MatchReader guard") }
                MTermLoader(newCInfo, body).map(_.map(SCaseDef(nameTypes, _, MTypeLoader.fromTpt(tpt).get)))
              }
            }): _*)

            cs.map(_.map(cases => SMatch(mTerm, cases, tpe)))
          }

        case _ =>
          unprocessedTree(tree, "MatchReader")
          Left(Failed)
      }
    }

  }

}
