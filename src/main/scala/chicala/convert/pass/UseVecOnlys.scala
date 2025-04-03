package chicala.convert.pass

import scala.math.Ordered
import scala.tools.nsc.Global

import chicala.util.Format
import chicala.ast.ChicalaAst
import chicala.ChicalaConfig.useVecOnly

import chicala.ast.util.Transformers
import chicala.util.Printer

trait UseVecOnlys extends ChicalaPasss with Transformers with Printer { self: ChicalaAst =>
  val global: Global
  import global._

  object UseVecOnly extends ChicalaPass {
    def apply(cClassDef: CClassDef): CClassDef = {
      cClassDef match {
        case m: ModuleDef =>
          m.copy(body = m.body.map(useVecOnlyTransformer(_)))
        case b: BundleDef =>
          b.copy(bundle = useVecOnlyTransformer.transformTypeT(b.bundle))
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
                .exists({ case (a, b) => a != b }) || cApply.tpe != newCApply.tpe
            ) {
              val op = newCApply.op
              val someHelperName = op match {
                case VecSelect | VecTake | Mux | AsSInt => Left(newCApply)
                case AsTypeOf =>
                  if (newCApply.tpe == newCApply.operands.head.tpe)
                    Left(newCApply.operands.head)
                  else
                    Left(newCApply)
                case AsUInt =>
                  newCApply.operands.head.tpe match {
                    case _: Vec => Left(newCApply.operands.head)
                    case _      => Right("chicala.lib.helper.BoolVec.AsUInt")
                  }
                case AsBool => Right("chicala.lib.helper.BoolVec.AsBool")

                case Not      => Right("chicala.lib.helper.BoolVec.Not")
                case Negative => Right("chicala.lib.helper.BoolVec.Negative")

                case Add      => Right("chicala.lib.helper.BoolVec.Add")
                case Minus    => Right("chicala.lib.helper.BoolVec.Minus")
                case Multiply => Right("chicala.lib.helper.BoolVec.Multiply")

                case And    => Right("chicala.lib.helper.BoolVec.And")
                case Or     => Right("chicala.lib.helper.BoolVec.Or")
                case Xor    => Right("chicala.lib.helper.BoolVec.Xor")
                case LShift => Right("chicala.lib.helper.BoolVec.LShift")
                case RShift => Right("chicala.lib.helper.BoolVec.RShift")

                case Equal     => Right("chicala.lib.helper.BoolVec.Equal")
                case GreaterEq => Right("chicala.lib.helper.BoolVec.GreaterEq")
                case NotEqual  => Right("chicala.lib.helper.BoolVec.NotEqual")

                case Slice =>
                  newCApply.tpe match {
                    case _: Bool => Left(newCApply.copy(op = VecSelect))
                    case _       => Right("chicala.lib.helper.BoolVec.Slice")
                  }

                case Cat  => Right("chicala.lib.helper.BoolVec.Cat")
                case Fill => Right("chicala.lib.helper.BoolVec.Fill")
                case Log2 => Right("chicala.lib.helper.BoolVec.Log2")
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
              SLib("chicala.lib.helper.BoolVec.Lit", StFunc),
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
                SLib("chicala.lib.helper.BoolVec.Enum", StFunc),
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
