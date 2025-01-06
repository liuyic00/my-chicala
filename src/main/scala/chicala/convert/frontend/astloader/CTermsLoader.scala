package chicala.convert.frontend

import scala.tools.nsc.Global

trait CTermsLoader { self: Scala2Reader =>
  val global: Global
  import global._

  object ConnectLoader extends Loader[Connect] {
    def apply(cInfo: CircuitInfo, tr: Tree): Option[(CircuitInfo, Option[Connect])] = {
      val (tree, _) = passThrough(tr)
      tree match {
        case Apply(Select(qualifier, TermName("$colon$eq")), args) if isChiselSignalType(qualifier) =>
          assert(args.length == 1, "should have one right expr")

          val (newCInfo, left :: right :: Nil) = MTermLoader.loadTerms(cInfo, List(qualifier, args.head))
          Some(newCInfo, Some(Connect(left, right)))
        case _ => None
      }
    }
  }
  object CApplyLoader extends Loader[CApply] {
    def apply(cInfo: CircuitInfo, tr: Tree): Option[(CircuitInfo, Option[CApply])] = {
      val (tree, tpt) = passThrough(tr)
      tree match {
        case Apply(Select(qualifier, name), args) if isChiselSignalType(qualifier) =>
          val opName = name.toString()
          COpLoader(opName) match {
            case Some(op) =>
              val tpe = MTypeLoader.fromTpt(tpt).get match {
                case StSeq(tparam: SignalType) => Vec(InferredSize, Node, tparam.setInferredWidth)
                case x: SignalType             => x.setInferredWidth
                case x =>
                  errorTree(tpt, s"Not a processable MType `${x}`")
                  SignalType.empty
              }
              val (newCInfo, operands) = MTermLoader.loadTerms(cInfo, qualifier :: args)
              Some((newCInfo, Some(CApply(op, tpe, operands))))
            case None =>
              unprocessedTree(tr, s"CApplyLoader `${opName}`")
              None
          }
        case a @ Apply(fun, args) => {
          val f     = passThrough(fun)._1
          val fName = f.toString()
          COpLoader(fName) match {
            case Some(op) =>
              val tpe                  = SignalTypeLoader.fromTpt(tpt).get.setInferredWidth
              val (newCInfo, operands) = MTermLoader.loadTerms(cInfo, args)
              Some((newCInfo, Some(CApply(op, tpe, operands))))
            case None => None
          }
        }
      }
    }
  }
  object WhenLoader extends Loader[When] {
    def apply(cInfo: CircuitInfo, tr: Tree): Option[(CircuitInfo, Option[When])] = {
      def bodyFromTree(cInfo: CircuitInfo, tr: Tree): List[MStatement] = {
        val (tree, _) = passThrough(tr)
        val treeBody = tree match {
          case Block(stats, expr) => stats :+ expr
          case tr                 => List(tr)
        }
        StatementReader.fromListTree(cInfo, treeBody)._2
      }
      def pushBackElseWhen(when: When, elseWhen: When): When = when match {
        case When(cond, whenp, otherp, true) =>
          When(
            cond,
            whenp,
            pushBackElseWhen(otherp.asInstanceOf[When], elseWhen),
            true
          )
        case When(cond, whenp, otherp, false) =>
          When(cond, whenp, elseWhen, true)
      }

      val (tree, _) = passThrough(tr)
      tree match {
        case Apply(Apply(cwa, condArgs), args) if isChisel3WhenApply(cwa) => {
          val (newCInfo, (cond: MTerm) :: whenp :: Nil) = loadsWithUpdateReaderInfo(cInfo)(
            MTermLoader(_, condArgs.head),
            StatementReader(_, args.head)
          )
          Some((newCInfo, Some(When(cond, whenp, EmptyMTerm))))
        }
        case Apply(Select(qualifier, TermName("otherwise")), args) => {
          val Some((cInfo1, Some(when))) = WhenLoader(cInfo, qualifier)
          val (cInfo2, otherp)           = StatementReader(cInfo.updatedWithReaderInfo(cInfo1), args.head).get
          Some((cInfo.updatedWithReaderInfo(cInfo2), Some(When(when.cond, when.whenp, otherp.get))))
        }
        case Apply(Apply(Select(qualifier, TermName("elsewhen")), condArgs), args) => {

          val (newCInfo, (when: When) :: (elseCond: MTerm) :: elseThen :: Nil) = loadsWithUpdateReaderInfo(cInfo)(
            WhenLoader(_, qualifier),
            MTermLoader(_, condArgs.head),
            StatementReader(_, args.head)
          )

          val whenElseWhen = pushBackElseWhen(when, When(elseCond, elseThen, EmptyMTerm))

          Some((newCInfo, Some(whenElseWhen)))
        }
        case _ => None
      }
    }
  }

  object SwitchLoader extends Loader[Switch] {
    def apply(cInfo: CircuitInfo, tr: Tree): Option[(CircuitInfo, Option[Switch])] = {
      val (tree, tpt) = passThrough(tr)
      if (!isChisel3UtilSwitchContextType(tpt)) return None

      tree match {
        case Apply(Apply(Select(qualifier, TermName("is")), vArgs), bodyArgs) =>
          val (newCInfo, (switch: Switch) :: v :: branchp :: Nil) = loadsWithUpdateReaderInfo(cInfo)(
            SwitchLoader(_, qualifier),
            MTermLoader(_, vArgs.head),
            MTermLoader(_, bodyArgs.head)
          )
          Some((newCInfo, Some(switch.appended(v, branchp))))
        case Apply(Select(New(t), termNames.CONSTRUCTOR), args) if isChisel3UtilSwitchContextType(t) =>
          val (newCInfo, cond :: Nil) = loadsWithUpdateReaderInfo(cInfo)(
            MTermLoader(_, args.head)
          )
          Some((newCInfo, Some(Switch(cond, List.empty))))
        case _ =>
          errorTree(tr, "Unknow structure in SwitchLoader")
          None
      }

    }
  }

  object AssertLoader extends Loader[Assert] {
    def apply(cInfo: CircuitInfo, tr: Tree): Option[(CircuitInfo, Option[Assert])] = {
      val (tree, _) = passThrough(tr)
      if (isReturnAssert(tree)) {
        tree match {
          case Apply(Ident(TermName("_applyWithSourceLinePrintable")), args) =>
            val (newCInfo, ast :: Nil) = MTermLoader.loadTerms(cInfo, args.head :: Nil)
            Some(newCInfo, Some(Assert(ast)))
          case _ => None
        }
      } else None
    }
  }

  object LitLoader extends Loader[Lit] {
    private def nameToSomeLitGen(name: Name): (STerm, CSize) => Option[Lit] = {
      name.toString() match {
        case "U" => (litExp, width) => Some(Lit(litExp, UInt(width, Node, Undirect)))
        case "S" => (litExp, width) => Some(Lit(litExp, SInt(width, Node, Undirect)))
        case "B" => (litExp, width) => Some(Lit(litExp, Bool(Node, Undirect)))
        case _   => (litExp, width) => None
      }
    }
    def apply(cInfo: CircuitInfo, tr: Tree): Option[(CircuitInfo, Option[Lit])] = {
      val (tree, tpt) = passThrough(tr)
      tree match {
        case Apply(Select(qualifier, name), args) if isChiselLiteralType(qualifier) => {
          // 0.U(1.W)
          val litTree = qualifier.asInstanceOf[Apply].args.head
          val litExp  = MTermLoader(cInfo, litTree).get._2.get.asInstanceOf[STerm]
          val width = SignalTypeLoader.getWidth(cInfo, args) match {
            case k: KnownSize => k
            case _            => InferredSize
          }
          nameToSomeLitGen(name)(litExp, width) match {
            case Some(lit) => Some((cInfo, Some(lit)))
            case None =>
              errorTree(tree, "Unknow name in CExp")
              None
          }

        }
        case Select(qualifier, name) if isChiselLiteralType(qualifier) => {
          // someInt.U without width
          val litTree = qualifier.asInstanceOf[Apply].args.head
          val litExp  = MTermLoader(cInfo, litTree).get._2.get.asInstanceOf[STerm]

          nameToSomeLitGen(name)(litExp, InferredSize) match {
            case Some(lit) => Some((cInfo, Some(lit)))
            case None =>
              errorTree(tree, "Unknow name in CExp")
              None
          }
        }
        case _ => None
      }
    }
  }
}
