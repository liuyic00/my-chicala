package chicala.convert.frontend

import scala.tools.nsc.Global

trait DefDefsReader { self: Scala2Reader =>
  val global: Global
  import global._

  object DefDefReader extends Loader[MDef] {
    def apply(cInfo: CircuitInfo, tr: Tree): LREitherSuccess[MDef] = {
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
            return Left(Failed)
          }

          val Right(Loaded(newCInfo, vpss: List[List[MValDef]])) = vparamssReader(cInfo, vparamss)

          if (name == termNames.CONSTRUCTOR) {
            // constructor of this class
            val vps = vpss.flatten.asInstanceOf[List[SValDef]]
            Right(Changed(cInfo.updatedWithReaderInfo(newCInfo).updatedParams(vps)))
          } else {
            // function
            StatementReader(newCInfo, rhs).flatMap {
              case Changed(cInfo) =>
                errorTree(rhs, "DefDefReader")
                Left(Failed)
              case Loaded(_, defp) => {
                assertError(defp.nonEmpty, rhs.pos, s"function $name should have body")
                val tpe = MTypeLoader.fromTpt(tpt).get
                Right(
                  Loaded(
                    cInfo.updatedFunc(name, tpe),
                    SDefDef(name, vpss, tpe, defp)
                  )
                )
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

  protected def vparamssReader(
      cInfo: CircuitInfo,
      vparamss: List[List[ValDef]]
  ): LREitherLoaded[List[List[MValDef]]] = {
    vparamss
      .foldLeft(Right(Loaded(cInfo, List.empty[List[MValDef]]))) { case (Right(Loaded(cf, ls)), vps) =>
        val Right(Loaded(ncf, nl)) = vps.foldLeft(Right(Loaded(cf, List.empty[MValDef]))) {
          case (Right(Loaded(c, l)), t) =>
            ValDefReader(c, t) match {
              case Right(Loaded(nc, svd: MValDef)) => Right(Loaded(nc, l :+ svd))
              case x =>
                unprocessedTree(t, "vparamssReader")
                Right(Loaded(c, l))
            }
        }
        Right(Loaded(ncf, ls :+ nl))
      }
  }

}
