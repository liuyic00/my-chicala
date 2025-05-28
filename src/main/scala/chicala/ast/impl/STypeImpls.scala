package chicala.ast.impl

import scala.tools.nsc.Global

import chicala.ast.ChicalaAst

trait STypeImpls { self: ChicalaAst =>
  val global: Global
  import global._

  trait STypeImpl { self: SType => }

  trait StTupleImpl { self: StTuple => }

  trait StSeqImpl   { self: StSeq =>   }
  trait StArrayImpl { self: StArray => }

  trait StWrappedImpl { self: StWrapped =>
    override def toString(): String = s"StWrapped(\"${str}\")"
  }

  trait KnownSizeObjImpl {
    def fromInt(size: Int): KnownSize = {
      KnownSize(SLiteral(size, StInt))
    }
  }
}
