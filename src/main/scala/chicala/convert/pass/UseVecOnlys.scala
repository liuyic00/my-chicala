package chicala.convert.pass

import scala.math.Ordered
import scala.tools.nsc.Global

import chicala.util.Format
import chicala.ast.ChicalaAst

import chicala.ast.util.{Transformers, Computes}
import chicala.util.Printer
import chicala.ChicalaConfig

trait UseVecOnlys extends ChicalaPasss with Transformers with Printer with Computes { self: ChicalaAst =>
  val global: Global
  import global._

  def sameCType(tpeA: MType, tpeB: MType): Boolean = {
    val r = (tpeA, tpeB) match {
      case (a: SignalType, b: SignalType) => a.nomalize == b.nomalize
      case (a, b)                         => a == b
    }
    r
  }

  object UseVecOnly extends ChicalaPass {
    def apply(cClassDef: CClassDef): CClassDef = {
      if (ChicalaConfig.useVecOnly) {
        cClassDef match {
          case m: ModuleDef =>
            m.copy(body = m.body.map(useVecOnlyTransformer(_)))
          case b: BundleDef =>
            b.copy(bundle = useVecOnlyTransformer.transformTypeT(b.bundle))
        }
      } else {
        cClassDef
      }
    }

    object useVecOnlyTransformer extends Transformer {
      override def transform(mStatement: MStatement): MStatement = {
        mStatement match {
          case cApply: CApply => {
            val newCApply = super.transform(mStatement).asInstanceOf[CApply]
            if (
              cApply.operands
                .map(_.tpe)
                .zip(newCApply.operands.map(_.tpe))
                .exists({ case (a, b) => a != b }) ||
              (
                !newCApply.tpe.isInstanceOf[Bool] &&
                  !newCApply.tpe.isInstanceOf[Bundle]
              )
            ) {
              val op = newCApply.op
              val someHelperName = op match {
                case VecSelect | VecTake | Mux | AsSInt => Left(newCApply)
                case AsTypeOf =>
                  if (sameCType(newCApply.tpe, newCApply.operands.head.tpe))
                    Left(newCApply.operands.head)
                  else
                    Left(newCApply)
                case AsUInt =>
                  newCApply.operands.head.tpe match {
                    case _: Vec => Left(newCApply.operands.head)
                    case _      => Right("h.bv.AsUInt")
                  }
                case AsBool => Right("h.bv.AsBool")

                case Not      => Right("h.bv.Not")
                case Negative => Right("h.bv.Negative")

                case Add      => Right("h.bv.Add")
                case Minus    => Right("h.bv.Minus")
                case Multiply => Right("h.bv.Multiply")

                case And    => Right("h.bv.And")
                case Or     => Right("h.bv.Or")
                case Xor    => Right("h.bv.Xor")
                case LShift => Right("h.bv.LShift")
                case RShift => Right("h.bv.RShift")

                case Equal     => Right("h.bv.Equal")
                case GreaterEq => Right("h.bv.GreaterEq")
                case NotEqual  => Right("h.bv.NotEqual")

                case Slice =>
                  newCApply.tpe match {
                    case _: Bool => Left(newCApply.copy(op = VecSelect))
                    case _       => Right("h.bv.Slice")
                  }

                case MuxLookup => Right("h.bv.MuxLookup")
                case Cat       => Right("h.bv.Cat")
                case Fill      => Right("h.bv.Fill")
                case Log2      => Right("h.bv.Log2")

                case _ =>
                  reportWarning(NoPosition, s"untransformed CApply in UseVecOnly: ${op}")
                  Left(newCApply)
              }
              someHelperName match {
                case Right(helperName) =>
                  SApply(
                    SLib(helperName, StFunc),
                    newCApply.operands,
                    newCApply.tpe
                  )
                case Left(value) => value
              }
            } else {
              newCApply
            }
          }
          case Lit(litExp, tpe: UInt) =>
            SApply(
              SLib("h.bv.Lit", StFunc),
              tpe.width match {
                case KnownSize(width) => List(litExp, width)
                case _                => List(litExp)
              },
              transformType(tpe)
            )
          case EnumDef(names, tpe) =>
            val boolVecType = transformType(tpe)
            SUnapplyDef(
              names,
              SApply(
                SLib("h.bv.Enum", StFunc),
                List(SLiteral(names.size, StInt)),
                boolVecType
              ),
              StTuple(List.fill(names.size)(boolVecType))
            )
          case SSelect(x, TermName("getWidth"), StInt) =>
            x.tpe match {
              case _: UInt => SSelect(transformStatementT(x), TermName("length"), StInt)
              case _       => super.transform(mStatement)
            }
          case SSelect(x, TermName("asUInt"), _) => transform(x)
          case SApply(
                SSelect(exp, TermName("apply"), StFunc),
                args,
                StWrapped("sv2chisel.helpers.SubWords[chisel3.Bool]")
              ) => {
            val x  = transformStatementT(exp)
            val as = args.map(transformStatementT(_))
            SApply(
              SLib("h.bv.Slice", StFunc),
              x :: as,
              x.tpe
                .asInstanceOf[Vec]
                .copy(size = KnownSize(leftRightSize(as(0).asInstanceOf[STerm], as(1).asInstanceOf[STerm])))
            )
          }
          case SApply(SLib(func, _), args, StWrapped("sv2chisel.helpers.SubWordable[chisel3.Bool]")) =>
            func match {
              case "sv2chisel.helpers.vecconvert.`package`.vecToSubwords" => transform(args.head)
              case _                                                      => super.transform(mStatement)
            }
          case _ => super.transform(mStatement)
        }
      }

      override def transformType(mType: MType): MType = {
        mType match {
          case UInt(width, physical, direction) =>
            Vec(width, physical, Bool(physical, direction))
          case _ => super.transformType(mType)
        }
      }
    }
  }
}
