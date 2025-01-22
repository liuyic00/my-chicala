package chicala.convert.frontend

import scala.tools.nsc.Global
import chicala.ast.impl.MTermImpls

trait ValDefsReader { self: Scala2Reader =>
  val global: Global
  import global._

  object ValDefReader extends Loader[MDef] {
    def apply(cInfo: CircuitInfo, tr: Tree): LREitherT[LRSuccess[MDef]] = {
      val (tree, _) = passThrough(tr)
      val ValDef(mods, nameTmp, tpt: TypeTree, rhs) = tree match {
        case v: ValDef => v
        case _         => unprocessedTree(tr, "ValDefReader #1"); return Left(Failed)
      }

      val name = nameTmp.stripSuffix(" ")

      if (isScala2UnapplyTmpValDef(tree)) {
        rhs match {
          case Match(Typed(Apply(cuea, number), _), _) if isChisel3UtilEnumApply(cuea) => { // EnumDef step 1
            val num = number.head.asInstanceOf[Literal].value.value.asInstanceOf[Int]
            val enumDef =
              EnumDef(
                List.empty,
                UInt(
                  KnownSize(SLiteral(BigInt(num - 1).bitLength, StInt)),
                  Node,
                  Undirect
                )
              )

            Right(Modified(cInfo.updatedEnumDefTmp(num, Some(name), Some(enumDef))))
          }
          case Match(Typed(rhs, _), _) => { // STupleUnapplyDef step 1
            val num = tpt.tpe.typeArgs.length
            val tpe = STypeLoader.fromTpt(tpt).get.asInstanceOf[StTuple]
            MTermLoader(cInfo, rhs).map { case ModifiedAndLoaded(newCInfo, cExp) =>
              Modified(
                newCInfo.updatedSUnapplyDefTmp(
                  num,
                  Some(name),
                  Some(SUnapplyDef(List.empty, cExp, tpe))
                )
              )
            }
          }
        }
      } else if (cInfo.numTmp > 0) {
        val tn  = cInfo.termNameTmp.get
        val num = cInfo.numTmp - 1
        if (
          passThrough(rhs)._1 match {
            case Select(Select(This(cInfo.name), tn: TermName), _) => true
            case Select(Ident(tn: TermName), _)                    => true
            case _                                                 => false
          }
        ) {
          if (cInfo.enumDefTmp.nonEmpty) { // EnumDef step 2
            val ed       = cInfo.enumDefTmp.get
            val enumDef  = EnumDef(ed.names :+ name, ed.tpe)
            val newCInfo = cInfo.updatedVal(name, enumDef.tpe)
            if (num == 0)
              Right(ModifiedAndLoaded(newCInfo.updatedEnumDefTmp(0, None, None), enumDef))
            else
              Right(Modified(newCInfo.updatedEnumDefTmp(num, Some(tn), Some(enumDef))))
          } else if (cInfo.sUnapplyDefTmp.nonEmpty) { // STupleUnapplyDef step 2
            val sud         = cInfo.sUnapplyDefTmp.get
            val sUnapplyDef = sud.copy(names = sud.names :+ name)
            val newCInfo    = cInfo.updatedVal(name, MTypeLoader.fromTpt(tpt).get)
            if (num == 0)
              Right(ModifiedAndLoaded(newCInfo.updatedSUnapplyDefTmp(0, None, None), sUnapplyDef))
            else
              Right(Modified(newCInfo.updatedSUnapplyDefTmp(num, Some(tn), Some(sUnapplyDef))))
          } else
            loadNodeDef(cInfo, name, rhs, mods.isMutable)
        } else
          loadNodeDef(cInfo, name, rhs, mods.isMutable)

      } else if (isChiselSignalType(tpt) || isChiselModuleType(tpt)) { // SignalDef and SubModuleDef
        passThrough(rhs)._1 match {
          // normal SignalDef and SubModuleDef
          case a @ Apply(func, args) =>
            if (isModuleThisIO(func, cInfo)) { // IoDef
              loadIoDef(cInfo, name, args)
            } else if (isChiselWireDefApply(func)) { // WireDef
              loadWireDef(cInfo, name, func, args, mods.isMutable)
            } else if (isChiselRegDefApply(func)) { // RegDef
              loadRegDef(cInfo, name, func, args)
            } else if (isChisel3ModuleDoApply(func)) { // SubModuleDef
              loadSubModuleDef(cInfo, name, args)
            } else { // NodeDef called function or operator
              loadNodeDef(cInfo, name, rhs, mods.isMutable)
            }
          case EmptyTree =>
            val tpe      = SignalTypeLoader.fromTpt(tpt).get
            val newCInfo = cInfo.updatedVal(name, tpe)
            val nodeDef  = NodeDef(name, tpe, EmptyMTerm, mods.isMutable)
            Right(ModifiedAndLoaded(newCInfo, nodeDef))
          case _ =>
            loadNodeDef(cInfo, name, rhs, mods.isMutable)
        }
      } else { // SValDef
        val name     = nameTmp.stripSuffix(" ")
        val tpe      = STypeLoader.fromTpt(tpt).get
        val newCInfo = cInfo.updatedVal(name, tpe)
        (MTermLoader(cInfo, rhs) match {
          case Right(ModifiedAndLoaded(_, r)) => Right(r)
          case Left(x: LRSkip)                => Right(EmptyMTerm)
          case Left(x: LRExit)                => Left(x)
        }).map { r =>
          val sValDef = SValDef(name, tpe, r, mods.isMutable)
          if (mods.isParamAccessor)
            if (mods.isParameter) ModifiedAndLoaded(newCInfo, sValDef)
            else Modified(newCInfo)
          else
            ModifiedAndLoaded(newCInfo, sValDef)
        }
      }
    }

    def loadIoDef(
        cInfo: CircuitInfo,
        name: TermName,
        args: List[Tree]
    ): LREitherT[ModifiedAndLoaded[IoDef]] = {
      val sigType: LREitherT[ModifiedAndLoaded[BundleDef]] = args.head match {
        case Block(stats, expr) => BundleDefLoader(cInfo, stats.head, "")
        case a @ Apply(Select(New(tpt), termNames.CONSTRUCTOR), aparams) =>
          val bundleFullName = tpt.tpe.toString()
          val someBundleDef  = cInfo.readerInfo.bundleDefs.get(bundleFullName)
          someBundleDef.toRight(DependentClassNotDef).flatMap { bundleDef =>
            MTermLoader.loadTerms(cInfo, aparams).flatMap { case ModifiedAndLoaded(newCInfo, mArgs) =>
              Right(ModifiedAndLoaded(newCInfo, bundleDef.applyArgs(mArgs)))
            }
          }
        case _ => Left(Failed) // this should not happed
      }

      sigType.map { case ModifiedAndLoaded(tmpCInfo, bundleDef) =>
        val bundle  = bundleDef.bundle.updatedPhysical(Io)
        val newInfo = tmpCInfo.updatedVal(name, bundle)
        val ioDef   = IoDef(name, bundle)
        ModifiedAndLoaded(newInfo, ioDef)
      }
    }

    def loadWireDef(
        cInfo: CircuitInfo,
        name: TermName,
        func: Tree,
        args: List[Tree],
        isVar: Boolean
    ): LREitherT[ModifiedAndLoaded[WireDef]] = {
      if (isChisel3WireApply(func)) {
        assertError(args.length == 1, func.pos, "Should have only 1 arg in Wire()")
        SignalTypeLoader(cInfo, args.head).map { case ModifiedAndLoaded(nInfo, st) =>
          val sigType  = st.updatedPhysical(Wire)
          val newCInfo = nInfo.updatedVal(name, sigType)
          ModifiedAndLoaded(newCInfo, WireDef(name, sigType))
        }
      } else if (isChisel3WireInitApply(func)) {
        MTermLoader(cInfo, args.head).map { case ModifiedAndLoaded(nInfo, init) =>
          val sigType = init.tpe.asInstanceOf[SignalType].updatedPhysical(Wire)
          val newInfo = nInfo.updatedVal(name, sigType)
          val wireDef = WireDef(name, sigType, Some(init), isVar)
          ModifiedAndLoaded(newInfo, wireDef)
        }
      } else if (isChisel3VecInitDoApply(func)) {
        assertError(args.length >= 1, func.pos, "Should have at last 1 arg in VecInit()")
        MTermLoader.loadTerms(cInfo, args).map { case ModifiedAndLoaded(newCInfo, mArgs) =>
          val init = SApply(SLib("scala.`package`.Seq.apply", StFunc), mArgs, StSeq(mArgs.head.tpe))
          val tpe = Vec(
            KnownSize.fromInt(mArgs.size),
            Wire,
            mArgs.head.tpe.asInstanceOf[SignalType]
          )
          val newInfo = newCInfo.updatedVal(name, tpe)
          ModifiedAndLoaded(newInfo, WireDef(name, tpe, Some(init)))
        }
      } else {
        reporter.error(func.pos, "Unknow WireDef function")
        Left(Failed)
      }

    }
    def loadRegDef(
        cInfo: CircuitInfo,
        name: TermName,
        func: Tree,
        args: List[Tree]
    ): LREitherT[ModifiedAndLoaded[RegDef]] = {
      if (isChisel3RegApply(func)) {
        assert(args.length == 1, "should have only 1 arg in Reg()")
        SignalTypeLoader(cInfo, args.head).map { case ModifiedAndLoaded(nInfo, st) =>
          val sigType  = st.updatedPhysical(Reg)
          val newCInfo = nInfo.updatedVal(name, sigType)
          ModifiedAndLoaded(newCInfo, RegDef(name, sigType))
        }
      } else if (isChisel3RegInitApply(func)) {
        if (args.length == 1) {
          MTermLoader(cInfo, args.head).map { case ModifiedAndLoaded(_, init) =>
            val signalInfo = init.tpe.asInstanceOf[SignalType].updatedPhysical(Reg)
            val newCInfo   = cInfo.updatedVal(name, signalInfo)
            ModifiedAndLoaded(newCInfo, RegDef(name, signalInfo, Some(init)))
          }
        } else {
          unprocessedTree(func, s"ValDefReader.loadRegDef with ${args.size} arg")
          Left(Failed)
        }
      } else if (isChisel3UtilRegEnableApply(func)) {
        if (args.length != 2)
          reporter.error(func.pos, "should have 2 args in RegEnable()")
        MTermLoader
          .loadTerms(cInfo, List(args.head, args.tail.head))
          .flatMap {
            case ModifiedAndLoaded(_, List(next, enable)) =>
              val signalInfo = next.tpe.asInstanceOf[SignalType].updatedPhysical(Reg)
              val newCInfo   = cInfo.updatedVal(name, signalInfo)
              Right(ModifiedAndLoaded(newCInfo, RegDef(name, signalInfo, None, Some(next), Some(enable))))
            case _ => loadMutilpleMatchError(args.head)
          }
      } else {
        reporter.error(func.pos, "Unknow RegDef function")
        Left(Failed)
      }
    }

    def loadNodeDef(
        cInfo: CircuitInfo,
        name: TermName,
        rhs: Tree,
        isVar: Boolean
    ): LREitherT[ModifiedAndLoaded[NodeDef]] = {
      MTermLoader(cInfo, rhs).map { case ModifiedAndLoaded(newCInfo, cExp) =>
        val signalInfo = cExp.tpe.asInstanceOf[SignalType].updatedPhysical(Node)
        val newInfo    = newCInfo.updatedVal(name, signalInfo)
        ModifiedAndLoaded(newInfo, NodeDef(name, signalInfo, cExp, isVar))
      }
    }

    def loadSubModuleDef(
        cInfo: CircuitInfo,
        name: TermName,
        args: List[Tree]
    ): LREitherT[ModifiedAndLoaded[SubModuleDef]] = {
      args.head match {
        case Apply(Select(New(tpt), termNames.CONSTRUCTOR), args) =>
          val moduleFullName = tpt.tpe.toString()
          val someModuleDef  = cInfo.readerInfo.moduleDefs.get(moduleFullName)
          someModuleDef match {
            case Some(value) =>
              val ioDef = value.ioDef
              val tpe   = SubModule(moduleFullName, ioDef)
              MTermLoader.loadTerms(cInfo, args).map { case ModifiedAndLoaded(_, mArgs) =>
                val subModuleDef = SubModuleDef(name, tpe, mArgs)
                ModifiedAndLoaded(cInfo.updatedVal(name, tpe), subModuleDef)
              }
            case None =>
              Left(DependentClassNotDef)
          }
        case _ =>
          unprocessedTree(args.head, "ValDefReader.loadSubModuleDef")
          Left(Failed)
      }
    }

  }

}
