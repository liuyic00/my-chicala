package chicala.convert.frontend

import scala.tools.nsc.Global

trait SelectsReader { self: Scala2Reader =>
  val global: Global
  import global._

  object SelectReader extends Reader[MTerm] {
    val sLibs: List[String] = List(
      "math.this.BigInt.int2bigInt",
      //
      "chisel3.util.log2Ceil.apply",
      "chisel3.util.log2Floor.apply",
      "chisel3.util.log2Up.apply",
      "chisel3.VecInit.do_tabulate",
      //
      "scala.Array.fill",
      "scala.`package`.Seq.apply",
      "scala.`package`.Range.apply",
      "scala.`package`.BigInt.apply",
      "scala.`package`.Nil",
      //
      "scala.Predef.intWrapper",
      "scala.Predef.ArrowAssoc",
      "scala.Predef.refArrayOps",
      // sv2chisel helpers
      "sv2chisel.helpers.vecconvert.`package`.vecToSubwords"
    )
    def apply(cInfo: CircuitInfo, tr: Tree): Either[LRError, Loaded[MTerm]] = {
      val (tree, tpt) = passThrough(tr)
      tree match {
        case s @ Select(qualifier, name: TermName) => {
          if (sLibs.contains(s.toString()))
            Right(Loaded(SLib(s.toString(), StFunc)))
          else if (isChiselSignalType(tpt)) {
            if (isChiselSignalType(qualifier) || isChiselModuleType(qualifier)) {
              COpLoader(name.toString()) match {
                case Some(op) => // unary operator
                  MTermLoader(cInfo, qualifier).map(_.mapValue { operand =>
                    CApply(op, List(operand))
                  })
                case None => // select from bundle / module io / This
                  // undefined operator will come to this case, but it is a bug
                  Right(Loaded(SignalRef(s, cInfo.getSignalType(s))))
              }
            } else if (isChiselLiteralType(qualifier)) {
              LitLoader.must(cInfo, tr)
            } else {
              MTermLoader(cInfo, qualifier).map(_.mapValue { from =>
                val tpe = MTypeLoader.fromTpt(tpt).get
                SSelect(from, name, tpe)
              })
            }
          } else { // SSelect SIdent
            val tpe = MTypeLoader.fromTpt(tpt).get
            qualifier match {
              case This(cInfo.name) =>
                Right(Loaded(SIdent(name, tpe)))
              case Ident(innerName: TermName) =>
                val sSelect = SSelect(SIdent(innerName, MTypeLoader.fromTpt(qualifier).get), name, tpe)
                Right(Loaded(sSelect))
              case t =>
                MTermLoader(cInfo, t).map(_.mapValue { from =>
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
