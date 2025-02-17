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
        case Lit(litExp, tpe)          => Lit(transformSTerm(litExp), transfromGroundType(tpe))
        case SignalRef(name, tpe)      => SignalRef(name, transformSignalType(tpe))
        case CApply(op, tpe, operands) => CApply(op, transformSignalType(tpe), operands.map(transformMTerm(_)))

        case Connect(left, expr) => Connect(transformMTerm(left), transformMTerm(expr))

        case GenCType(tpe) => GenCType(transformTypeT(tpe))

        case When(cond, whenp, otherp, hasElseWhen) =>
          When(transformMTerm(cond), transform(whenp), transform(otherp), hasElseWhen)
        case Assert(exp) => Assert(transformMTerm(exp))
        case Switch(cond, branchs) =>
          Switch(transformMTerm(cond), branchs.map(x => (transformMTerm(x._1), transform(x._2))))
        case SubModuleRun(name, inputRefs, outputNames, moduleType, inputSignals, outputSignals) =>
          SubModuleRun(
            name,
            inputRefs.map(transformStatementT),
            outputNames.map(transformTermName),
            transformTypeT(moduleType),
            inputSignals.map({ case (n, t) => (n, transformTypeT(t)) }),
            outputSignals.map({ case (n, t) => (n, transformTypeT(t)) })
          )

        // STerm
        case SApply(fun, args, tpe)   => SApply(transformSTerm(fun), args.map(transformMTerm(_)), transformType(tpe))
        case SSelect(from, name, tpe) => SSelect(transformMTerm(from), transformTermName(name), transformType(tpe))
        case SBlock(body, tpe)        => SBlock(body.map(transform(_)), transformType(tpe))
        case SLiteral(value, tpe)     => SLiteral(value, transformType(tpe))
        case SIdent(name, tpe)        => SIdent(transformTermName(name), transformType(tpe))
        case SIf(cond, thenp, elsep, tpe) =>
          SIf(transformSTerm(cond), transformMTerm(thenp), transformMTerm(elsep), transformType(tpe))
        case SMatch(selector, cases, tpe) =>
          SMatch(transformMTerm(selector), cases.map(transformSCaseDef(_)), transformType(tpe))
        case SCaseDef(tupleNames, casep, tpe) =>
          SCaseDef(
            tupleNames.map(x => (transformTermName(x._1), transformType(x._2))),
            transformMTerm(casep),
            transformType(tpe)
          )
        case STuple(args, tpe) => STuple(args.map(transformMTerm(_)), transformStTuple(tpe))

        case SLib(name, tpe)          => SLib(name, transformSType(tpe))
        case SFunction(vparams, body) => SFunction(vparams.map(transformMValDef(_)), body)
        case SAssign(lhs, rhs)        => SAssign(transformMTerm(lhs), transformMTerm(rhs))

        case EmptyMTerm => EmptyMTerm

        // CValDef
        case SubModuleDef(name, tpe, args) =>
          SubModuleDef(transformTermName(name), transformTypeT(tpe), args.map(transformMTerm(_)))

        case IoDef(name, tpe) => IoDef(transformTermName(name), transformTypeT(tpe))
        case WireDef(name, tpe, someInit, isVar) =>
          WireDef(transformTermName(name), transformTypeT(tpe), someInit.map(transformStatementT), isVar)
        case RegDef(name, tpe, someInit, someNext, someEnable) =>
          RegDef(
            transformTermName(name),
            transformTypeT(tpe),
            someInit.map(transformStatementT),
            someNext.map(transformStatementT),
            someEnable.map(transformStatementT)
          )
        case NodeDef(name, tpe, rhs, isVar) =>
          NodeDef(transformTermName(name), transformTypeT(tpe), transformStatementT(rhs), isVar)

        // SValDef
        case SValDef(name, tpe, rhs, isVar) =>
          SValDef(transformTermName(name), transformTypeT(tpe), transformStatementT(rhs), isVar)

        // other Def
        case EnumDef(names, tpe) => EnumDef(names.map(transformTermName), transformTypeT(tpe))
        case SUnapplyDef(names, rhs, tpe) =>
          SUnapplyDef(names.map(transformTermName), transformStatementT(rhs), transformTypeT(tpe))

        case SDefDef(name, vparamss, tpe, defp) =>
          SDefDef(
            transformTermName(name),
            vparamss.map(_.map(transformStatementT)),
            transformType(tpe),
            transform(defp)
          )
      }
    }
    def transformStatementT[T <: MStatement](statement: T): T = transform(statement).asInstanceOf[T]
    def transformMTerm(mTerm: MTerm): MTerm                   = transform(mTerm).asInstanceOf[MTerm]
    def transformSTerm(sTerm: STerm): STerm                   = transform(sTerm).asInstanceOf[STerm]
    def transformSCaseDef(sCaseDef: SCaseDef): SCaseDef       = transform(sCaseDef).asInstanceOf[SCaseDef]
    def transformMValDef(mValDef: MValDef): MValDef           = transform(mValDef).asInstanceOf[MValDef]

    def transformType(mType: MType): MType = {
      mType match {
        case Bundle(physical, signals) =>
          Bundle(
            physical,
            signals.map({ case (termName, signalType) =>
              (termName, transformTypeT(signalType))
            })
          )
        case Vec(size, physical, tparam) =>
          Vec(size, physical, transformTypeT(tparam))
        case _ => mType
      }
    }

    def transformTypeT[T <: MType](tpe: T): T = transformType(tpe).asInstanceOf[T]

    def transfromGroundType(groundType: GroundType): GroundType = transformType(groundType).asInstanceOf[GroundType]
    def transformSignalType(signalType: SignalType): SignalType = transformType(signalType).asInstanceOf[SignalType]
    def transformUInt(uInt: UInt): UInt                         = transformType(uInt).asInstanceOf[UInt]
    def transformStTuple(stTuple: StTuple): StTuple             = transformType(stTuple).asInstanceOf[StTuple]
    def transformSType(sType: SType): SType                     = transformType(sType).asInstanceOf[SType]

    def transformTermName(termName: TermName): TermName = termName
  }
}
