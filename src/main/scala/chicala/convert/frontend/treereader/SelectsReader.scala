package chicala.convert.frontend

import scala.tools.nsc.Global

trait SelectsReader { self: Scala2Reader =>
  val global: Global
  import global._

  object SelectReader extends Loader[MTerm] {
    val sLibs: List[String] = List(
      "math.this.BigInt.int2bigInt",
      //
      "chisel3.util.log2Ceil.apply",
      "chisel3.util.log2Floor.apply",
      "chisel3.util.log2Up.apply",
      //
      "scala.Array.fill",
      "scala.`package`.Seq.apply",
      "scala.`package`.Range.apply",
      "scala.`package`.BigInt.apply",
      "scala.`package`.Nil",
      //
      "scala.Predef.intWrapper",
      "scala.Predef.ArrowAssoc",
      "scala.Predef.refArrayOps"
    )
    def apply(cInfo: CircuitInfo, tr: Tree): LREitherLoaded[MTerm] = {
      val (tree, tpt) = passThrough(tr)
      tree match {
        case s @ Select(qualifier, name: TermName) => {
          if (sLibs.contains(s.toString()))
            Right(Loaded(cInfo, SLib(s.toString(), StFunc)))
          else if (isChiselSignalType(tpt)) {
            if (isChiselSignalType(qualifier) || isChiselModuleType(qualifier)) {
              COpLoader(name.toString()) match {
                case Some(op) => // unary operator
                  MTermLoader(cInfo, qualifier).map(_.map { operand =>
                    CApply(op, SignalTypeLoader.fromTpt(tpt).get, List(operand))
                  })
                case None => // select from bundle / module io / This
                  // undefined operator will come to this case, but it is a bug
                  Right(Loaded(cInfo, SignalRef(s, cInfo.getSignalType(s))))
              }
            } else if (isChiselLiteralType(qualifier)) {
              LitLoader(cInfo, tr).flatMap {
                case l: Loaded[_] => Right(l)
                case NotThis =>
                  unprocessedTree(tr, "SelectReader")
                  Left(Failed)
                case Changed(cInfo) =>
                  errorTree(tr, "SelectReader")
                  Left(Failed)
              }
            } else {
              MTermLoader(cInfo, qualifier).map(_.map { from =>
                val tpe = MTypeLoader.fromTpt(tpt).get
                SSelect(from, name, tpe)
              })
            }
          } else { // SSelect SIdent
            val tpe = MTypeLoader.fromTpt(tpt).get
            qualifier match {
              case This(cInfo.name) =>
                Right(Loaded(cInfo, SIdent(name, tpe)))
              case Ident(innerName: TermName) =>
                val sSelect = SSelect(SIdent(innerName, MTypeLoader.fromTpt(qualifier).get), name, tpe)
                Right(Loaded(cInfo, sSelect))
              case t =>
                MTermLoader(cInfo, t).map(_.map { from =>
                  SSelect(from, name, tpe)
                })
            }
          }
        }
        case _ =>
          unprocessedTree(tree, "SelectReader")
          Left(Failed)
      }

    }

  }

}
