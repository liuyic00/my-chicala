package chicala.convert.frontend

import scala.tools.nsc.Global
import scala.collection.SeqMap

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
        .map(_.value)
        .left
        .map {
          case NotThis =>
            unprocessedTree(tree, "CClassDefLoader")
            Failed
          case x: LRError => x
        }
    }
  }

  object ModuleDefLoader {
    def apply(tree: Tree, pkg: String)(implicit readerInfo: ReaderInfo): Either[LRAllLeft, Loaded[ModuleDef]] =
      tree match {
        // only Class inherits chisel3.Module directly
        case ClassDef(mods, name, tparams, Template(parents, self, body)) if parents.exists {
              case Select(Ident(TermName("chisel3")), TypeName("Module")) => true
              case _                                                      => false
            } =>
          StatementReader.fromListTree(CircuitInfo(name), body).map { case ModifiedAndLoaded(cInfo, cBody) =>
            Loaded(ModuleDef(name, cInfo.params, cBody, pkg))
          }
        case _ => Left(NotThis)
      }
  }

  object BundleDefLoader {
    def apply(tree: Tree, pkg: String)(implicit readerInfo: ReaderInfo): Either[LRError, Loaded[BundleDef]] = {
      val name = tree.asInstanceOf[ClassDef].name
      BundleDefLoader(CircuitInfo(name), tree, pkg).map { case Loaded(bundleDef) =>
        Loaded(bundleDef)
      }
    }
    def apply(cInfo: CircuitInfo, tree: Tree, pkg: String): Either[LRError, Loaded[BundleDef]] = {
      tree match {
        // only Class inherits chisel3.Bundle directly
        case ClassDef(mods, name, tparams, Template(parents, self, body)) if parents.exists {
              case Select(Ident(TermName("chisel3")), TypeName("Bundle")) => true
              case _                                                      => false
            } => {
          var vps = List.empty[SValDef]
          val eitherInfoSignals =
            body.foldLeft(
              Right(ModifiedAndLoaded(cInfo, SeqMap.empty)): Either[LRError, ModifiedAndLoaded[
                SeqMap[TermName, SignalType]
              ]]
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
                      SignalTypeLoader.must(nowCInfo, rhs) match {
                        case Right(Loaded(sigType)) =>
                          Right(ModifiedAndLoaded(nowCInfo, nowSet ++ SeqMap(name -> sigType)))
                        case Left(f: LRExit) => Left(f)
                        case Left(_: LRSkip) => Right(ModifiedAndLoaded(nowCInfo, nowSet))
                      }
                    } else {
                      ValDefReader(nowCInfo, tr) match {
                        case Right(x: LRModified[_]) => Right(ModifiedAndLoaded(x.cInfo, nowSet))
                        case Right(Loaded(_)) =>
                          errorTree(tr, "BundleDefLoader")
                          Right(ModifiedAndLoaded(nowCInfo, nowSet))
                        case Left(f: LRExit) => Left(f)
                        case Left(_: LRSkip) => Right(ModifiedAndLoaded(nowCInfo, nowSet))
                      }
                    }
                  case _ =>
                    Right(ModifiedAndLoaded(nowCInfo, nowSet))
                }
              }
              case (Left(f), tr) => Left(f)
            }

          eitherInfoSignals.map { case ModifiedAndLoaded(_, signals) =>
            Loaded(BundleDef(name, vps, Bundle(Node, signals), pkg))
          }
        }
      }
    }
  }
}
