package chicala.convert.pass

import scala.math.Ordered
import scala.tools.nsc.Global

import chicala.util.Format
import chicala.ast.ChicalaAst
import chicala.ast.util._

trait ExpandSubModuleDefs extends ChicalaPasss with Transformers with Replacers { self: ChicalaAst =>
  val global: Global
  import global._

  object ExpandSubModuleDef extends ChicalaPass {
    def apply(cClassDef: CClassDef): CClassDef = {
      cClassDef match {
        case m: ModuleDef => expandSubModuleDef(m)
        case x            => x
      }
    }

    def expandSubModuleDef(m: ModuleDef): ModuleDef = {
      val transformer = new expandSubModuleDefTransformer()
      m.copy(body = transformer.expandList(m.body, Some(m.name)))
    }

    class expandSubModuleDefTransformer() extends Transformer {

      def expandList(
          body: List[MStatement],
          selectFromModule: Option[TypeName]
      ): List[MStatement] = {
        var repMap = Map.empty[MStatement, MStatement]
        body
          .map({
            Replacer(repMap)(_) match {
              case subModuleDef @ SubModuleDef(name, tpe, args) =>
                val (ioSigDefs, subModuleRun, newRepMap) =
                  expand(subModuleDef, selectFromModule)
                repMap = repMap ++ newRepMap
                (subModuleDef :: ioSigDefs) :+ subModuleRun
              case x => List(transform(Replacer(repMap)(x)))
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
        val outputRefs = outputSignals.map({ case (name, tpe) =>
          SignalRef(
            optionSelectThis(TermName(flattenName(name))),
            tpe.updatedPhysical(Wire).updatedDriction(Undirect)
          )
        })

        val subModuleRun = SubModuleRun(
          optionSelectThis(subModuleName),
          inputRefs,
          outputRefs,
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

      override def transform(mStatement: MStatement): MStatement = mStatement match {
        case d @ SDefDef(_, _, _, defp) => d.copy(defp = expandSBlockOrOther(defp))
        case f @ SFunction(_, funcp)    => f.copy(funcp = expandSBlockOrOther(funcp).asInstanceOf[MTerm])
        case b @ SBlock(body, tpe)      => b.copy(body = expandList(body, None))
        case x                          => super.transform(x)
      }

      private def expandSBlockOrOther(bodyp: MStatement): MStatement = {
        bodyp match {
          case SBlock(body, tpe) => SBlock(expandList(body, None), tpe)
          case x =>
            expandList(List(x), None) match {
              case head :: Nil => head
              case newBody     => SBlock(newBody, newBody.last.tpe)
            }
        }
      }

    }

  }
}
