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
      "scala.`package`.Seq.fill",
      "scala.`package`.Seq.apply",
      "scala.`package`.Range.apply",
      "scala.`package`.BigInt.apply",
      "scala.`package`.Nil",
      //
      "scala.Predef.intWrapper",
      "scala.Predef.ArrowAssoc",
      "scala.Predef.refArrayOps",
      // sv2chisel helpers
      "sv2chisel.helpers.vecconvert.`package`.vecToSubwords",
      "sv2chisel.helpers.vecconvert.`package`.subwordsToVec"
    )
    def apply(cInfo: CircuitInfo, tr: Tree): Either[LRError, Loaded[MTerm]] = {
      val (tree, tpt) = passThrough(tr)
      tree match {
        // get value of <init>$default$n
        case Select(Select(_, _), termName) if termName.decode.startsWith("<init>$default$") => {
          val defaultMethodSym = tree.symbol
          val ownerSym         = defaultMethodSym.owner
          for {
            methodSym <- ownerSym.info.decls
              .find(_.name == defaultMethodSym.name)
              .toRight({
                reportError(tree.pos, s"not found $tree [@SelectReader_1]")
                Failed
              })
            defdef <- (
              // find DefDef through attachments
              methodSym.attachments.all
                .collectFirst { case d: global.DefDef if d.symbol == methodSym => d }
              )
              .orElse(
                // or find DefDef through current run units,
                // if the attachment is not available
                currentRun.units.toList.flatMap { unit =>
                  unit.body.collect { case d: global.DefDef if d.symbol == methodSym => d }
                }.headOption
              )
              .toRight({
                reportError(tree.pos, s"not found $tree [@SelectReader_2]")
                Failed
              })
            loaded <- MTermLoader.must(cInfo, defdef.rhs)
          } yield {
            loaded
          }
        }
        case s @ Select(qualifier, name: TermName) => {
          if (sLibs.contains(s.toString()))
            Right(Loaded(SLib(s.toString(), StFunc)))
          else if (isChiselSignalType(tpt)) {
            if (isChiselSignalType(qualifier) || isChiselModuleType(qualifier)) {
              COpLoader(name.toString()) match {
                case Some(op) => // unary operator
                  MTermLoader
                    .must(cInfo, qualifier)
                    .map(_.mapValue { operand =>
                      CApply(op, List(operand))
                    })
                case None => // select from bundle / module io / This
                  // undefined operator will come to this case, but it is a bug
                  Right(Loaded(SignalRef(s, cInfo.getSignalType(s))))
              }
            } else if (isChiselLiteralType(qualifier)) {
              LitLoader.must(cInfo, tr)
            } else {
              MTermLoader
                .must(cInfo, qualifier)
                .map(_.mapValue { from =>
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
                MTermLoader
                  .must(cInfo, t)
                  .map(_.mapValue { from =>
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
