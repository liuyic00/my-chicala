package chicala.convert.frontend

import scala.tools.nsc.Global

trait CClassDefsLoader { self: Scala2Reader =>
  val global: Global
  import global._

  object CClassDefLoader {
    def apply(tree: Tree, pkg: String)(implicit readerInfo: ReaderInfo): Either[LRError, CClassDef] = {
      LRSuccess
        .getFirst(
          List(
            () => ModuleDefLoader(tree, pkg),
            () => BundleDefLoader(tree, pkg)
          )
        )
        .flatMap {
          case ModifiedAndLoaded(_, cClassDef) =>
            Right(cClassDef)
          case _ =>
            unprocessedTree(tree, "CClassDefLoader")
            Left(Failed)
        }
    }
  }

  object ModuleDefLoader {
    def apply(tree: Tree, pkg: String)(implicit readerInfo: ReaderInfo): LREither[ModuleDef] = tree match {
      // only Class inherits chisel3.Module directly
      case ClassDef(mods, name, tparams, Template(parents, self, body)) if parents.exists {
            case Select(Ident(TermName("chisel3")), TypeName("Module")) => true
            case _                                                      => false
          } =>
        StatementReader.fromListTree(CircuitInfo(name), body).map { case ModifiedAndLoaded(cInfo, cBody) =>
          ModifiedAndLoaded(CircuitInfo.empty, ModuleDef(name, cInfo.params, cBody, pkg))
        }
      case _ => Right(NotThis)
    }
  }

  object BundleDefLoader {
    def apply(tree: Tree, pkg: String)(implicit readerInfo: ReaderInfo): LREitherT[ModifiedAndLoaded[BundleDef]] = {
      val name = tree.asInstanceOf[ClassDef].name
      BundleDefLoader(CircuitInfo(name), tree, pkg).map { case ModifiedAndLoaded(cInfo, bundleDef) =>
        ModifiedAndLoaded(CircuitInfo.empty, bundleDef)
      }
    }
    def apply(cInfo: CircuitInfo, tree: Tree, pkg: String): LREitherT[ModifiedAndLoaded[BundleDef]] = {
      tree match {
        // only Class inherits chisel3.Bundle directly
        case ClassDef(mods, name, tparams, Template(parents, self, body)) if parents.exists {
              case Select(Ident(TermName("chisel3")), TypeName("Bundle")) => true
              case _                                                      => false
            } => {
          var vps = List.empty[SValDef]
          val eitherInfoSignals =
            body.foldLeft(
              Right(ModifiedAndLoaded(cInfo, Map.empty)): LREitherT[ModifiedAndLoaded[Map[TermName, SignalType]]]
            ) {
              case (Right(ModifiedAndLoaded(nowCInfo, nowSet)), tr) => {
                tr match {
                  case d @ DefDef(mods, termNames.CONSTRUCTOR, tparams, vparamss, tpt, rhs) =>
                    val Right(ModifiedAndLoaded(nCInfo, vpss)) = vparamssReader(nowCInfo, vparamss)
                    vps = vpss.flatten.asInstanceOf[List[SValDef]]
                    Right(ModifiedAndLoaded(nCInfo, nowSet))
                  case ValDef(mods, nameTmp, tpt, rhs) =>
                    val name = nameTmp.stripSuffix(" ")
                    if (isChiselSignalType(tpt)) {
                      SignalTypeLoader(nowCInfo, rhs) match {
                        case Right(ModifiedAndLoaded(cf, sigType)) =>
                          Right(ModifiedAndLoaded(cf, nowSet + (name -> sigType)))
                        case Left(f: LRExit) => Left(f)
                        case Left(_: LRSkip) => Right(ModifiedAndLoaded(nowCInfo, nowSet))
                      }
                    } else {
                      ValDefReader(nowCInfo, tr) match {
                        case Right(x: LRModified[_]) => Right(ModifiedAndLoaded(x.cInfo, nowSet))
                        case Left(f: LRExit)         => Left(f)
                        case Left(_: LRSkip)         => Right(ModifiedAndLoaded(nowCInfo, nowSet))
                      }
                    }
                  case _ =>
                    Right(ModifiedAndLoaded(nowCInfo, nowSet))
                }
              }
              case (Left(f), tr) => Left(f)
            }

          eitherInfoSignals.map { case ModifiedAndLoaded(newCInfo, signals) =>
            ModifiedAndLoaded(newCInfo, BundleDef(name, vps, Bundle(Node, signals), pkg))
          }
        }
      }
    }
  }
}
