package chicala.convert.frontend

import scala.tools.nsc.Global

trait DefDefsReader { self: Scala2Reader =>
  val global: Global
  import global._

  object DefDefReader extends Reader[MDef] {
    def apply(cInfo: CircuitInfo, tr: Tree): Either[LRError, LRModified[MDef]] = {
      val tree = passThrough(tr)._1
      tree match {
        case d @ DefDef(mods, nameTmp, tparams, vparamss, tpt: TypeTree, rhs) => {
          val name = nameTmp.stripSuffix(" ")

          // accessor of val and var
          if (mods.hasAccessorFlag) {
            val valName =
              if (name.toString().endsWith("_$eq"))
                TermName(name.toString().dropRight(4))
              else name
            assertError(cInfo.contains(valName), d.pos, "accessor of val should record in cInfo")
            Left(Failed)
          } else if (name == termNames.CONSTRUCTOR) {
            // constructor of this class
            vparamssReader(cInfo, vparamss).map {
              case ModifiedAndLoaded(_, vpss: List[List[MValDef]]) => {
                val vps = vpss.flatten.asInstanceOf[List[SValDef]]
                Modified(cInfo.updatedParams(vps))
              }
            }
          } else {
            // function
            vparamssReader(cInfo, vparamss).flatMap { case ModifiedAndLoaded(newCInfo, vpss: List[List[MValDef]]) =>
              StatementReader(newCInfo, rhs).flatMap {
                case x: LRLoaded[_] => {
                  val defp = x.value
                  assertError(defp.nonEmpty, rhs.pos, s"function $name should have body")
                  val tpe = MTypeLoader.fromTpt(tpt).get
                  Right(
                    ModifiedAndLoaded(
                      cInfo.updatedFunc(name, tpe),
                      SDefDef(name, vpss, tpe, defp)
                    )
                  )
                }
                case _ =>
                  errorTree(rhs, "DefDefReader")
                  Left(Failed)
              }
            }
          }
        }
        case _ =>
          unprocessedTree(tree, "DefDefReader")
          Left(Failed)
      }

    }
  }

  /** Read vparamss of a function.
    *
    * Params will `updatedVal` in `cInfo`. However, the CONSTRUCTOR need
    * `updatedParams` outside manually.
    */
  protected def vparamssReader(
      cInfo: CircuitInfo,
      vparamss: List[List[ValDef]]
  ): Either[LRError, ModifiedAndLoaded[List[List[MValDef]]]] = {
    vparamss
      .foldLeft(Right(ModifiedAndLoaded(cInfo, List.empty[List[MValDef]]))) {
        case (Right(ModifiedAndLoaded(cf, ls)), vps) =>
          val Right(ModifiedAndLoaded(ncf, nl)) = vps.foldLeft(Right(ModifiedAndLoaded(cf, List.empty[MValDef]))) {
            case (Right(ModifiedAndLoaded(c, l)), t) =>
              ValDefReader(c, t) match {
                case Right(ModifiedAndLoaded(nc, svd: MValDef)) => Right(ModifiedAndLoaded(nc, l :+ svd))
                case x =>
                  unprocessedTree(t, "vparamssReader")
                  Right(ModifiedAndLoaded(c, l))
              }
          }
          Right(ModifiedAndLoaded(ncf, ls :+ nl))
      }
  }

}
