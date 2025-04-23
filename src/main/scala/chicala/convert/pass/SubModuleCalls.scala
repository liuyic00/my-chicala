package chicala.convert.pass

import scala.math.Ordered
import scala.tools.nsc.Global

import chicala.util.Format
import chicala.ast.ChicalaAst
import chicala.ast.util._

trait SubModuleCalls extends ChicalaPasss with Transformers with Replacers { self: ChicalaAst =>
  val global: Global
  import global._

  object SubModuleCall extends ChicalaPass {
    def apply(cClassDef: CClassDef): CClassDef = {
      cClassDef match {
        case m: ModuleDef => subModuleCall(m)
        case x            => x
      }
    }

    def subModuleCall(moduleDef: ModuleDef): ModuleDef = {
      moduleDef.copy(body = expandTopList(moduleDef.body, false)(moduleDef.name))
    }

    def expandTopList(
        body: List[MStatement],
        isFuncTop: Boolean
    )(implicit moduleName: TypeName): List[MStatement] = {
      var repMap = Map.empty[MStatement, MStatement]
      body
        .map({
          Replacer(repMap)(_) match {
            case s @ SubModuleDef(name, tpe, args) =>
              val (ioSigDefs, subModuleRun, newRepMap) =
                expand(s, if (isFuncTop) None else Some(moduleName))
              repMap = repMap ++ newRepMap
              (s :: ioSigDefs) :+ subModuleRun
            case x =>
              val subModuleExpander = new SubModuleExpander()
              val newStatement      = subModuleExpander(Replacer(repMap)(x))
              subModuleExpander.pullOutDefs :+ newStatement
          }
        })
        .flatten
    }

    private def expand(
        subModuleDef: SubModuleDef,
        selectFromModule: Option[TypeName]
    ): (List[MStatement], SubModuleRun, Map[MStatement, MStatement]) = {
      def optionSelectThis(name: TermName) = selectFromModule match {
        case Some(moduleName) => Select(This(moduleName), name)
        case None             => Ident(name)
      }

      ()
      val SubModuleDef(subModuleName, subModuleType, subModuleArgs) = subModuleDef
      val subModuleIoDefs                                           = subModuleType.ioDefs

      def flattenName(name: String) = s"${subModuleName}_${name}"

      val argsReplaceIn = {
        val argReplaceMap = subModuleType.vparams
          .map(sValDef => SIdent(sValDef.name, sValDef.tpe): MStatement)
          .zip(subModuleArgs: List[MStatement])
          .toMap
        Replacer(argReplaceMap)
      }
      val signals = subModuleIoDefs
        .flatMap(ioDef => ioDef.tpe.flatten(ioDef.name.toString()))
        .map { case (name, tpe) => (name, argsReplaceIn.transformTypeT(tpe)) }
      val inputSignals  = signals.filter({ case (name, tpe) => tpe.isInput })
      val outputSignals = signals.filter({ case (name, tpe) => tpe.isOutput })

      val ioSigDefs = signals.map({ case (name, tpe) =>
        WireDef(
          TermName(s"${subModuleName}_${name}"),
          tpe.updatedPhysical(Wire).updatedDriction(Undirect),
          None
        )
      })

      val inputRefs = inputSignals.map({ case (name, tpe) =>
        SignalRef(
          optionSelectThis(TermName(flattenName(name))),
          tpe.updatedPhysical(Wire).updatedDriction(Undirect)
        )
      })

      val outputNames = outputSignals.map({ case (name, _) =>
        TermName(flattenName(name))
      })

      val subModuleRun = SubModuleRun(
        optionSelectThis(subModuleName),
        inputRefs,
        outputNames,
        subModuleType,
        inputSignals,
        outputSignals
      )

      val replaceMap = {
        def selectIt(selectPath: List[TermName]): (List[Tree], String) = {
          val selects = selectPath match {
            case head :: next =>
              List(optionSelectThis(head), Ident(head)) // only need one?
                .map(x => next.foldLeft(x)((x, y) => Select(x, y)))
            case Nil => List.empty
          }
          val flattenName = selectPath.mkString("_")
          (selects, flattenName)
        }
        def getReplaceMap(oldTpe: SignalType, prefixs: List[TermName]): Map[MStatement, MStatement] = {
          oldTpe match {
            case _: GroundType | _: Vec =>
              val (selects, flattenName) = selectIt(prefixs)
              val newType = argsReplaceIn.transformTypeT(
                if (oldTpe.isInput)
                  oldTpe.updatedPhysical(Wire).updatedDriction(Undirect)
                else
                  oldTpe.updatedPhysical(Node).updatedDriction(Undirect)
              )
              selects
                .map(x =>
                  SignalRef(x, oldTpe)
                    -> SignalRef(optionSelectThis(TermName(flattenName)), newType)
                )
                .toMap
            case Bundle(physical, signals) =>
              signals
                .map({ case (name, oldTpe) => getReplaceMap(oldTpe, prefixs :+ name) })
                .reduce(_ ++ _)
          }
        }

        subModuleIoDefs
          .map(ioDef => getReplaceMap(ioDef.tpe, List(subModuleName, ioDef.name)))
          .reduce(_ ++ _)
      }

      (ioSigDefs, subModuleRun, replaceMap)
    }

    class SubModuleExpander()(implicit moduleName: TypeName) extends Transformer {
      var pullOutDefs: List[MStatement] = List.empty

      override def transform(mStatement: MStatement): MStatement = mStatement match {
        case d @ SDefDef(_, _, _, defp) => d.copy(defp = expandTopSBlockOrOther(defp, true))
        case f @ SFunction(_, funcp)    => f.copy(funcp = expandTopSBlockOrOther(funcp, true).asInstanceOf[MTerm])
        case b @ SBlock(body, tpe)      => b.copy(body = expandSubList(body))
        case x                          => super.transform(x)
      }

      private def expandTopSBlockOrOther(bodyp: MStatement, isFuncTop: Boolean): MStatement = {
        bodyp match {
          case SBlock(body, tpe) => SBlock(expandTopList(body, isFuncTop), tpe)
          case x                 => transform(x)
        }
      }

      private def expandSubList(body: List[MStatement]): List[MStatement] = {
        var repMap = Map.empty[MStatement, MStatement]
        body.map({
          Replacer(repMap)(_) match {
            case s @ SubModuleDef(name, tpe, args) =>
              val (ioSigDefs, subModuleRun, newRepMap) = expand(s, Some(moduleName))
              pullOutDefs = pullOutDefs ++ (s :: ioSigDefs)
              repMap = repMap ++ newRepMap
              subModuleRun
            case x => super.transform(x)
          }
        })
      }

    }

  }
}
