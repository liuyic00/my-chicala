package chicala.convert.pass

import scala.tools.nsc.Global

import chicala.ast.ChicalaAst

import chicala.ast.util.Transformers
import chicala.ChicalaConfig

trait UseNestedCats extends ChicalaPasss with Transformers { self: ChicalaAst =>
  val global: Global
  import global._

  object UseNestedCat extends ChicalaPass {
    def apply(cClassDef: CClassDef): CClassDef = {
      if (ChicalaConfig.useNestedCat) {
        cClassDef match {
          case m: ModuleDef => m.copy(body = m.body.map(useNestedCat(_)))
          case b: BundleDef => b
        }
      } else
        cClassDef
    }

    object useNestedCat extends Transformer {
      def nestedCat(operands: List[MTerm]): MTerm = {
        operands match {
          case a :: b :: c :: mayBeNil => CApply(Cat, List(a, nestedCat(b :: c :: mayBeNil)))
          case _                       => CApply(Cat, operands)
        }
      }
      def nestedHBvCat(operands: List[MTerm], tpe: Vec): MTerm = {
        operands match {
          case a :: b :: c :: mayBeNil =>
            val someAWidth = a.tpe match {
              case _: Bool => Some(SLiteral(1, StInt))
              case v: Vec =>
                v.size match {
                  case KnownSize(width) => Some(width)
                  case _                => None
                }
              case _ => None
            }
            val someWidth = tpe.size match {
              case KnownSize(width) => Some(width)
              case _                => None
            }

            val newTpe = (someWidth, someAWidth) match {
              case (Some(width), Some(awidth)) =>
                tpe.copy(size = KnownSize(simplify(minus(width, awidth))))
              case _ => tpe
            }

            SApply(SLib("h.bv.Cat", StFunc), List(a, nestedHBvCat(b :: c :: mayBeNil, newTpe)), tpe)
          case _ =>
            SApply(SLib("h.bv.Cat", StFunc), operands, tpe)
        }
      }
      override def transform(mStatement: MStatement): MStatement = mStatement match {
        case CApply(Cat, operands) if operands.length > 2 =>
          nestedCat(operands.map(transformT[MTerm](_)))
        case SApply(SLib("h.bv.Cat", StFunc), operands, tpe: Vec) =>
          nestedHBvCat(operands.map(transformT[MTerm](_)), tpe)
        case _ => super.transform(mStatement)
      }
    }
  }

}
