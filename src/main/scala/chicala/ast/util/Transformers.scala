package chicala.ast.util

import scala.tools.nsc.Global

import chicala.ast.ChicalaAst

trait Transformers { self: ChicalaAst =>
  val global: Global
  import global._

  class Transformer {
    def apply(mStatement: MStatement): MStatement = transform(mStatement)
    def transform(mStatement: MStatement): MStatement = {
      mStatement match {
        // CTerm
        case Lit(litExp, tpe)     => Lit(transformT(litExp), transformTypeT(tpe))
        case SignalRef(name, tpe) => SignalRef(transformTree(name), transformTypeT(tpe))
        case CApply(op, operands) => CApply(op, operands.map(transformT))

        case Connect(left, expr) => Connect(transformT(left), transformT(expr))

        case GenCType(tpe) => GenCType(transformTypeT(tpe))

        case When(cond, whenp, otherp, hasElseWhen) =>
          When(transformT(cond), transform(whenp), transform(otherp), hasElseWhen)
        case Assert(exp) => Assert(transformT(exp))
        case Switch(cond, branchs) =>
          Switch(transformT(cond), branchs.map(x => (transformT(x._1), transform(x._2))))
        case SubModuleRun(name, inputRefs, outputRefs, moduleType, inputSignals, outputSignals) =>
          SubModuleRun(
            name,
            inputRefs.map(transformT),
            outputRefs.map(transformT),
            transformTypeT(moduleType),
            inputSignals.map({ case (n, t) => (n, transformTypeT(t)) }),
            outputSignals.map({ case (n, t) => (n, transformTypeT(t)) })
          )

        // STerm
        case SApply(fun, args, tpe) =>
          SApply(transformT(fun), args.map(transformT), transformType(tpe))
        case SSelect(from, name, tpe) => SSelect(transformT(from), transformTermName(name), transformType(tpe))
        case SBlock(body, tpe)        => SBlock(body.map(transform), transformType(tpe))
        case SLiteral(value, tpe)     => SLiteral(value, transformType(tpe))
        case SIdent(name, tpe)        => SIdent(transformTermName(name), transformType(tpe))
        case SIf(cond, thenp, elsep, tpe) =>
          SIf(transformT(cond), transformT(thenp), transformT(elsep), transformType(tpe))
        case SMatch(selector, cases, tpe) =>
          SMatch(transformT(selector), cases.map(transformT), transformType(tpe))
        case SCaseDef(tupleNames, casep, tpe) =>
          SCaseDef(
            tupleNames.map(x => (transformTermName(x._1), transformType(x._2))),
            transformT(casep),
            transformType(tpe)
          )
        case STuple(args, tpe) => STuple(args.map(transformT), transformTypeT(tpe))

        case SLib(name, tpe)          => SLib(name, transformTypeT(tpe))
        case SFunction(vparams, body) => SFunction(vparams.map(transformT), transformT(body))
        case SAssign(lhs, rhs)        => SAssign(transformT(lhs), transformT(rhs))

        case EmptyMTerm   => EmptyMTerm
        case Comment(msg) => Comment(msg)

        // CValDef
        case SubModuleDef(name, tpe, args) =>
          SubModuleDef(transformTermName(name), transformTypeT(tpe), args.map(transformT(_)))

        case IoDef(name, tpe) => IoDef(transformTermName(name), transformTypeT(tpe))
        case WireDef(name, tpe, someInit, isVar) =>
          WireDef(transformTermName(name), transformTypeT(tpe), someInit.map(transformT), isVar)
        case RegDef(name, tpe, someInit, someNext, someEnable) =>
          RegDef(
            transformTermName(name),
            transformTypeT(tpe),
            someInit.map(transformT),
            someNext.map(transformT),
            someEnable.map(transformT)
          )
        case NodeDef(name, tpe, rhs, isVar) =>
          NodeDef(transformTermName(name), transformTypeT(tpe), transformT(rhs), isVar)

        // SValDef
        case SValDef(name, tpe, rhs, isVar) =>
          SValDef(transformTermName(name), transformTypeT(tpe), transformT(rhs), isVar)

        // other Def
        case EnumDef(names, tpe) => EnumDef(names.map(transformTermName), transformTypeT(tpe))
        case SUnapplyDef(names, rhs, tpe) =>
          SUnapplyDef(names.map(transformTermName), transformT(rhs), transformTypeT(tpe))

        case SDefDef(name, vparamss, tpe, defp) =>
          SDefDef(
            transformTermName(name),
            vparamss.map(_.map(transformT)),
            transformType(tpe),
            transform(defp)
          )
      }
    }

    def transformType(mType: MType): MType = {
      mType match {
        case Bundle(physical, signals) =>
          Bundle(
            physical,
            signals.map({ case (termName, signalType) =>
              (termName, transformTypeT(signalType))
            })
          )
        case Vec(size, physical, tparam)      => Vec(transformCSize(size), physical, transformTypeT(tparam))
        case UInt(width, physical, direction) => UInt(transformCSize(width), physical, direction)
        case SInt(width, physical, direction) => SInt(transformCSize(width), physical, direction)
        case Bool(physical, direction)        => Bool(physical, direction)
        case SubModule(fullName, ioDefs, vparams) =>
          SubModule(fullName, ioDefs.map(transformT), vparams.map(transformT))

        case StTuple(tparams) => StTuple(tparams.map(transformTypeT))
        case StSeq(tparam)    => StSeq(transformTypeT(tparam))
        case StArray(tparam)  => StArray(transformTypeT(tparam))

        case _ => mType
      }
    }

    def transformTermName(termName: TermName): TermName = termName

    def transformCSize(cSize: CSize): CSize = cSize match {
      case KnownSize(width) => KnownSize(transformT(width))
      case _                => cSize
    }

    // TODO: sould use tree.transform() instead of this
    def transformTree(tree: Tree): Tree = tree match {
      case Select(qualifier, name: TermName) =>
        Select(transformTree(qualifier), transformTermName(name))
      case _ => tree
    }

    def transformT[T <: MStatement](statement: T): T = transform(statement).asInstanceOf[T]

    def transformTypeT[T <: MType](tpe: T): T = transformType(tpe).asInstanceOf[T]

  }
}
