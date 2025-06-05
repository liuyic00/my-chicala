package chicala.ast.impl

import scala.tools.nsc.Global

import chicala.ast.ChicalaAst
import chicala.ast.util.Computes

trait CTermImpls extends Computes { self: ChicalaAst =>
  val global: Global
  import global._

  trait LitImpl { self: Lit =>
    val relatedIdents = litExp.relatedIdents ++ RelatedIdents.used(tpe.usedVal)
  }
  trait SignalRefImpl { self: SignalRef =>
    val relatedIdents = {
      val dependency = tpe.allSignals(name.toString(), false)
      RelatedIdents(Set.empty, Set.empty, dependency, Set.empty, Set.empty)
    }
  }
  trait CApplyImpl { self: CApply =>
    val tpe: SignalType = {
      op match {
        case _: TypeNotChanged => operands.head.tpe.asInstanceOf[SignalType].nomalize
        case _: TypeInferred =>
          operands.head.tpe.asInstanceOf[SignalType].nomalize.setInferredWidth
        case _: ToBool => Bool.empty
        case _: ToUInt => UInt.empty.setInferredWidth
        case _: ToSInt => SInt.empty.setInferredWidth
        // TypeChanged
        case Slice =>
          operands match {
            case x :: i :: Nil => Bool.empty
            case x :: l :: r :: Nil =>
              UInt(KnownSize(leftRightSize(l.asInstanceOf[STerm], r.asInstanceOf[STerm])), Node, Undirect)
            case _ =>
              reportError(NoPosition, "Slice should have at most 2 operands")
              Bool.empty
          }
        case AsTypeOf  => operands(1).tpe.asInstanceOf[SignalType].nomalize
        case VecSelect => operands.head.tpe.asInstanceOf[Vec].tparam.nomalize
        case Mux       => operands(1).tpe.asInstanceOf[SignalType].setInferredWidth
        case MuxLookup => operands(1).tpe.asInstanceOf[SignalType].setInferredWidth
        case _         => Bool.empty
      }
    }
    val relatedIdents: RelatedIdents =
      op match {
        case _: CNotDependOp =>
          operands.head.relatedIdents ++
            RelatedIdents.used(operands.tail.map(_.relatedIdents.dependency).reduce(_ ++ _))
        case _ =>
          operands.map(_.relatedIdents).reduce(_ ++ _)
      }
    override def toString: String =
      s"${op.toString}(${operands.map(_.toString).reduce(_ + ", " + _)}, ${tpe})"

  }

  trait ConnectImpl { self: Connect =>
    val tpe = left.tpe.asInstanceOf[SignalType].updatedPhysical(Node)
    val relatedIdents: RelatedIdents = {
      val fully = left match {
        case SignalRef(name, tpe) => tpe.allSignals(name.toString(), true)
        case _                    => left.relatedIdents.dependency
      }
      RelatedIdents(fully, Set.empty, Set.empty, Set.empty, Set.empty)
    } ++ expr.relatedIdents

    def expands: List[Connect] = {
      (left, expr) match {
        case (SignalRef(ln, lt: Bundle), SignalRef(rn, rt: Bundle)) =>
          val subNames = lt.signals.keys.filter(rt.signals.contains).toList
          subNames
            .map(n =>
              Connect(
                SignalRef(Select(ln, n), lt.signals(n)),
                SignalRef(Select(rn, n), rt.signals(n))
              ).expands
            )
            .flatten
        case _ => List(this)
      }
    }
  }

  trait GenCTypeImpl { self: GenCType =>
    val relatedIdents = RelatedIdents.used(tpe.usedVal)
  }

  trait WhenImpl { self: When =>
    val tpe = EmptyMType
    val relatedIdents: RelatedIdents = {
      val whenRI  = whenp.relatedIdents
      val otherRI = otherp.relatedIdents
      val condRI  = cond.relatedIdents

      val sum = whenRI ++ otherRI ++ condRI

      val fully      = whenRI.fully.intersect(otherRI.fully)
      val partially  = whenRI.partially ++ otherRI.partially ++ (whenRI.fully ++ otherRI.fully -- fully)
      val dependency = sum.dependency
      val updated    = sum.updated
      val used       = sum.used

      RelatedIdents(fully, partially, dependency, updated, used)
    }
  }
  trait AssertImpl { self: Assert =>
    val tpe           = EmptyMType
    val relatedIdents = exp.relatedIdents
  }
  trait SwitchImpl { self: Switch =>
    val tpe = EmptyMType
    def appended(v: MTerm, branchp: MStatement): Switch = {
      this.copy(branchs = branchs.appended((v, branchp)))
    }
    val relatedIdents = cond.relatedIdents ++
      branchs.map(x => x._1.relatedIdents ++ x._2.relatedIdents).foldLeft(RelatedIdents.empty)(_ ++ _)
  }
  trait SubModuleRunImpl { self: SubModuleRun =>
    val tpe = EmptyMType

    val relatedIdents: RelatedIdents = {
      val nameStr   = name.toString()
      val fully     = outputRefs.map(_.relatedIdents.dependency).reduce(_ ++ _)
      val partially = Set.empty[String]
      val dependency = Set(nameStr) ++
        inputRefs.map(_.relatedIdents.dependency).reduce(_ ++ _)

      RelatedIdents(fully, partially, dependency, Set.empty, Set.empty)
    }
  }

}
