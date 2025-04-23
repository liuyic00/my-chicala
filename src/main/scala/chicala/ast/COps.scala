package chicala.ast

import scala.tools.nsc.Global

trait COps { self: ChicalaAst =>
  val global: Global
  import global._

  // operators
  sealed abstract class COp

  sealed trait TypeNotChanged extends COp
  sealed trait TypeChanged    extends COp
  sealed trait TypeInferred   extends COp
  sealed trait ToBool         extends TypeChanged
  sealed trait ToUInt         extends TypeChanged
  sealed trait ToSInt         extends TypeChanged

  sealed abstract class CFrontOp extends COp
  case object LogiNot            extends CFrontOp with TypeNotChanged // `!a`
  case object Not                extends CFrontOp with TypeNotChanged // ~a
  case object Negative           extends CFrontOp with TypeInferred   // -a

  sealed abstract class CBinaryOp extends COp

  case object Add      extends CBinaryOp with TypeInferred // +
  case object Minus    extends CBinaryOp with TypeInferred // -
  case object Multiply extends CBinaryOp with TypeInferred // `*`

  case object And    extends CBinaryOp with TypeNotChanged // &
  case object Or     extends CBinaryOp with TypeNotChanged // |
  case object Xor    extends CBinaryOp with TypeNotChanged // ^
  case object LShift extends CBinaryOp with TypeInferred   // <<
  case object RShift extends CBinaryOp with TypeInferred   // >>

  case object Equal     extends CBinaryOp with ToBool // ===
  case object NotEqual  extends CBinaryOp with ToBool // =/=
  case object GreaterEq extends CBinaryOp with ToBool // >=

  case object LogiAnd extends CBinaryOp with TypeNotChanged // &&
  case object LogiOr  extends CBinaryOp with TypeNotChanged // ||

  sealed abstract class CBackOp extends COp

  case object Slice extends CBackOp with TypeChanged // a()

  case object VecSelect extends CBackOp with TypeChanged  // vec()
  case object VecTake   extends CBackOp with TypeInferred // vec.take()
  case object VecLast   extends CBackOp with TypeInferred // vec.last

  case object AsUInt extends CBackOp with ToUInt // .asUInt
  case object AsSInt extends CBackOp with ToSInt // .asSInt
  case object AsBool extends CBackOp with ToBool // .asBool

  sealed abstract class CNotDependOp extends CBackOp

  case object AsTypeOf extends CNotDependOp with TypeChanged // .asTypeOf()

  sealed abstract class CUtilOp extends COp
  case object Mux               extends CUtilOp with TypeChanged
  case object MuxLookup         extends CUtilOp with TypeChanged
  case object Cat               extends CUtilOp with ToUInt
  case object Fill              extends CUtilOp with ToUInt
  case object Log2              extends CUtilOp with TypeInferred

}
