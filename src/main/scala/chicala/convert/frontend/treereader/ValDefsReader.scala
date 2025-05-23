package chicala.convert.frontend

import scala.tools.nsc.Global
import chicala.ast.impl.MTermImpls

trait ValDefsReader { self: Scala2Reader =>
  val global: Global
  import global._

  object ValDefReader extends Reader[MDef] {
    def apply(cInfo: CircuitInfo, tr: Tree): Either[LRError, LRSuccess[MDef]] = {
      // TODO: has Loaded?
      val (tree, _) = passThrough(tr)
      tree match {
        case ValDef(mods, nameTmp, tpt: TypeTree, rhs) => {

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
                MTermLoader.must(cInfo, rhs).map { case Loaded(cExp) =>
                  Modified(
                    cInfo.updatedSUnapplyDefTmp(
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
            (MTermLoader.must(cInfo, rhs) match {
              case Right(Loaded(r)) => Right(r)
              case Left(x: LRSkip)  => Right(EmptyMTerm)
              case Left(x: LRExit)  => Left(x)
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
        case _ =>
          unprocessedTree(tr, "ValDefReader #1")
          Left(Failed)
      }
    }

    def loadIoDef(
        cInfo: CircuitInfo,
        name: TermName,
        args: List[Tree]
    ): Either[LRError, ModifiedAndLoaded[IoDef]] = {
      SignalTypeLoader.must(cInfo, args.head).map { case Loaded(sType) =>
        val t = sType.updatedPhysical(Io)
        ModifiedAndLoaded(
          cInfo.updatedVal(name, t),
          IoDef(name, t)
        )
      }
    }

    def loadWireDef(
        cInfo: CircuitInfo,
        name: TermName,
        func: Tree,
        args: List[Tree],
        isVar: Boolean
    ): Either[LRError, ModifiedAndLoaded[WireDef]] = {
      if (isChisel3WireApply(func)) {
        assertError(args.length == 1, func.pos, "Should have only 1 arg in Wire()")
        SignalTypeLoader.must(cInfo, args.head).map { case Loaded(st) =>
          val sigType = st.updatedPhysical(Wire)
          ModifiedAndLoaded(cInfo.updatedVal(name, sigType), WireDef(name, sigType))
        }
      } else if (isChisel3WireInitApply(func) || isChisel3WireDefaultApply(func)) {
        if (args.size == 1) {
          MTermLoader.must(cInfo, args.head).map { case Loaded(init) =>
            val sigType = init.tpe.asInstanceOf[SignalType].updatedPhysical(Wire)
            ModifiedAndLoaded(
              cInfo.updatedVal(name, sigType),
              WireDef(name, sigType, Some(init), isVar)
            )
          }
        } else if (args.size == 2) {
          for {
            sigType <- SignalTypeLoader.must(cInfo, args.head).map(_.value.updatedPhysical(Wire))
            init    <- MTermLoader.must(cInfo, args.tail.head).map(_.value)
          } yield {
            ModifiedAndLoaded(
              cInfo.updatedVal(name, sigType),
              WireDef(name, sigType, Some(init), isVar)
            )
          }
        } else {
          unprocessedTree(func, "ValDefReader.loadWireDef")
          Left(Failed)
        }
      } else if (isChisel3VecInitDoApply(func)) {
        assertError(args.length >= 1, func.pos, "Should have at last 1 arg in VecInit()")
        MTermLoader.loadTerms(cInfo, args).map { case Loaded(mArgs) =>
          val init = SApply(SLib("scala.`package`.Seq.apply", StFunc), mArgs, StSeq(mArgs.head.tpe))
          val tpe = Vec(
            KnownSize.fromInt(mArgs.size),
            Wire,
            mArgs.head.tpe.asInstanceOf[SignalType]
          )
          ModifiedAndLoaded(cInfo.updatedVal(name, tpe), WireDef(name, tpe, Some(init)))
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
    ): Either[LRError, ModifiedAndLoaded[RegDef]] = {
      if (isChisel3RegApply(func)) {
        assert(args.length == 1, "should have only 1 arg in Reg()")
        SignalTypeLoader.must(cInfo, args.head).map { case Loaded(st) =>
          val sigType = st.updatedPhysical(Reg)
          ModifiedAndLoaded(cInfo.updatedVal(name, sigType), RegDef(name, sigType))
        }
      } else if (isChisel3RegInitApply(func)) {
        if (args.length == 1) {
          MTermLoader.must(cInfo, args.head).map { case Loaded(init) =>
            val signalInfo = init.tpe.asInstanceOf[SignalType].updatedPhysical(Reg)
            ModifiedAndLoaded(
              cInfo.updatedVal(name, signalInfo),
              RegDef(name, signalInfo, Some(init))
            )
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
            case Loaded(List(next, enable)) =>
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
    ): Either[LRError, ModifiedAndLoaded[NodeDef]] = {
      MTermLoader.must(cInfo, rhs).map { case Loaded(cExp) =>
        val signalInfo = cExp.tpe.asInstanceOf[SignalType].updatedPhysical(Node)
        ModifiedAndLoaded(
          cInfo.updatedVal(name, signalInfo),
          NodeDef(name, signalInfo, cExp, isVar)
        )
      }
    }

    def loadSubModuleDef(
        cInfo: CircuitInfo,
        name: TermName,
        args: List[Tree]
    ): Either[LRError, ModifiedAndLoaded[SubModuleDef]] = {
      args.head match {
        case Apply(Select(New(tpt), termNames.CONSTRUCTOR), args) =>
          val moduleFullName = tpt.tpe.toString()
          for {
            moduleDef <- cInfo.readerInfo.moduleDefs
              .get(moduleFullName)
              .toRight(DependentClassNotDef(moduleFullName))
            mArgs <- MTermLoader.loadTerms(cInfo, args).map(_.value)
          } yield {
            val ioDefs       = moduleDef.ioDefs
            val tpe          = SubModule(moduleFullName, ioDefs, moduleDef.vparams)
            val subModuleDef = SubModuleDef(name, tpe, mArgs)
            ModifiedAndLoaded(cInfo.updatedVal(name, tpe), subModuleDef)
          }
        case _ =>
          unprocessedTree(args.head, "ValDefReader.loadSubModuleDef")
          Left(Failed)
      }
    }

  }

}
